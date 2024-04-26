// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.ExecutionEvent.*
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.StepExecutionState
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait ExecutionEventService[F[_]] {

  def atomRange(
    atomId: Atom.Id
  )(using Transaction[F]): F[Option[TimestampInterval]]

  def stepRange(
    stepId: Step.Id
  )(using Transaction[F]): F[Option[TimestampInterval]]

  def visitRange(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[TimestampInterval]]

  def insertAtomEvent(
    atomId:    Atom.Id,
    atomStage: AtomStage
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertDatasetEvent(
    datasetId:    Dataset.Id,
    datasetStage: DatasetStage
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertSequenceEvent(
    visitId: Visit.Id,
    command: SequenceCommand
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertSlewEvent(
    visitId:   Visit.Id,
    slewStage: SlewStage
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertStepEvent(
    stepId:    Step.Id,
    stepStage: StepStage
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def selectAtomExecutionState(
    atomId: Atom.Id
  )(using Transaction[F]): F[AtomExecutionState]

  def selectStepExecutionState(
    stepId: Step.Id
  )(using Transaction[F]): F[StepExecutionState]

}

object ExecutionEventService {
  extension (x: ResultT.type) {
    def liftF[F[_], A](fa: F[A])(implicit F: cats.Functor[F]): ResultT[F, A] =
      ResultT(fa.map(_.success))
  }

  def instantiate[F[_]: Concurrent](using Services[F]): ExecutionEventService[F] =
    new ExecutionEventService[F] {

      override def atomRange(
        atomId: Atom.Id
      )(using Transaction[F]): F[Option[TimestampInterval]] =
        session.unique(Statements.SelectAtomRange)(atomId)

      override def stepRange(
        stepId: Step.Id
      )(using Transaction[F]): F[Option[TimestampInterval]] =
        session.unique(Statements.SelectStepRange)(stepId)

      override def visitRange(
        visitId: Visit.Id
      )(using Transaction[F]): F[Option[TimestampInterval]] =
        session.unique(Statements.SelectVisitRange)(visitId)

      def insertAtomEvent(
        atomId:    Atom.Id,
        atomStage: AtomStage
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {
        def invalidAtom: OdbError.InvalidAtom =
          OdbError.InvalidAtom(atomId, Some(s"Atom '$atomId' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id, Visit.Id)]] =
          session
            .option(Statements.InsertAtomEvent)(atomId, atomStage)
            .map(_.toResult(invalidAtom.asProblem))
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) => invalidAtom.asFailureF
            }

        (for {
          e <- ResultT(insert)
          (eid, time, oid, vid) = e
          _ <- ResultT.liftF(timeAccountingService.update(vid))
        } yield AtomEvent(eid, time, oid, vid, atomId, atomStage)).value
      }

      override def insertDatasetEvent(
        datasetId:    Dataset.Id,
        datasetStage: DatasetStage
      )(using xa: Transaction[F], sa: Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        def invalidDataset: OdbError.InvalidDataset =
          OdbError.InvalidDataset(datasetId, Some(s"Dataset '${datasetId.show}' not found"))

        val insertEvent: F[Result[(Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Step.Id)]] =
          session
            .option(Statements.InsertDatasetEvent)(datasetId, datasetStage)
            .map(_.toResult(invalidDataset.asProblem))
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) => invalidDataset.asFailureF
            }

        // Best-effort to set the dataset time accordingly.  This can fail (leaving the timestamps
        // unchanged) if there is an end event but no start or if the end time comes before the
        // start.
        def setDatasetTime(t: Timestamp): F[Unit] = {
          def setWith(f: (Dataset.Id, Timestamp) => F[Unit]): F[Unit] =
            for {
              s <- xa.savepoint
              _ <- f(datasetId, t).recoverWith {
                case SqlState.CheckViolation(_) => xa.rollback(s).void
              }
            } yield ()

          // StartExpose signals the start of the dataset, EndWrite the end.
          datasetStage match {
            case DatasetStage.StartExpose => setWith(services.datasetService.setStartTime)
            case DatasetStage.EndWrite    => setWith(services.datasetService.setEndTime)
            case _                        => ().pure
          }
        }

        (for {
          e <- ResultT(insertEvent)
          (eid, time, oid, vid, aid, sid) = e
          _ <- ResultT.liftF(setDatasetTime(time))
          _ <- ResultT.liftF(timeAccountingService.update(vid))
        } yield DatasetEvent(eid, time, oid, vid, aid, sid, datasetId, datasetStage)).value
      }

      override def insertSequenceEvent(
        visitId: Visit.Id,
        command: SequenceCommand
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        def invalidVisit: OdbError.InvalidVisit =
          OdbError.InvalidVisit(visitId, Some(s"Visit '$visitId' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id)]] =
          session
            .option(Statements.InsertSequenceEvent)(visitId, command)
            .map(_.toResult(invalidVisit.asProblem))
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) => invalidVisit.asFailureF
            }

        (for {
          e <- ResultT(insert)
          (eid, time, oid) = e
          _ <- ResultT.liftF(timeAccountingService.update(visitId))
        } yield SequenceEvent(eid, time, oid, visitId, command)).value
      }

      override def insertSlewEvent(
        visitId:   Visit.Id,
        slewStage: SlewStage
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        def invalidVisit: OdbError.InvalidVisit =
          OdbError.InvalidVisit(visitId, Some(s"Visit '$visitId' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id)]] =
          session
            .option(Statements.InsertSlewEvent)(visitId, slewStage)
            .map(_.toResult(invalidVisit.asProblem))
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) => invalidVisit.asFailureF
            }

        (for {
          e <- ResultT(insert)
          (eid, time, oid) = e
          _ <- ResultT.liftF(timeAccountingService.update(visitId))
        } yield SlewEvent(eid, time, oid, visitId, slewStage)).value
      }

      override def insertStepEvent(
        stepId:       Step.Id,
        stepStage:    StepStage
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        def invalidStep: OdbError.InvalidStep =
          OdbError.InvalidStep(stepId, Some(s"Step '$stepId' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id, Visit.Id, Atom.Id)]] =
          session
            .option(Statements.InsertStepEvent)(stepId, stepStage)
            .map(_.toResult(invalidStep.asProblem))
            .recoverWith {
              case SqlState.ForeignKeyViolation(_) => invalidStep.asFailureF
            }

        def setStepCompleted(time: Timestamp): F[Unit] =
          services
            .sequenceService
            .setStepCompleted(stepId, Option.when(stepStage === StepStage.EndStep)(time))

        (for {
          e <- ResultT(insert)
          (eid, time, oid, vid, aid) = e
          _ <- ResultT.liftF(setStepCompleted(time))
          _ <- ResultT.liftF(timeAccountingService.update(vid))
        } yield StepEvent(eid, time, oid, vid, aid, stepId, stepStage)).value
      }

      override def selectAtomExecutionState(
        atomId: Atom.Id
      )(using Transaction[F]): F[AtomExecutionState] =
        session
          .option(Statements.SelectMaxAtomStage)(atomId)
          .map {
            case None                    => AtomExecutionState.NotStarted
            case Some(AtomStage.EndAtom) => AtomExecutionState.Completed
            case _                       => AtomExecutionState.Ongoing
          }

      override def selectStepExecutionState(
        stepId: Step.Id
      )(using Transaction[F]): F[StepExecutionState] =
        session
          .option(Statements.SelectMaxStepStage)(stepId)
          .map {
            case None                    => StepExecutionState.NotStarted
            case Some(StepStage.EndStep) => StepExecutionState.Completed
            case Some(StepStage.Abort)   => StepExecutionState.Aborted
            case Some(StepStage.Stop)    => StepExecutionState.Stopped
            case _                       => StepExecutionState.Ongoing
          }
    }

  object Statements {

    private val timestamp_interval: Decoder[TimestampInterval] =
      (core_timestamp *: core_timestamp).map { (min, max) =>
        TimestampInterval.between(min, max)
      }

    val SelectAtomRange: Query[Atom.Id, Option[TimestampInterval]] =
      sql"""
        SELECT
          MIN(e.c_received),
          MAX(e.c_received)
        FROM
          t_execution_event e
        INNER JOIN t_step_record s ON
          s.c_step_id = e.c_step_id
        WHERE
          s.c_atom_id = $atom_id
      """.query(timestamp_interval.opt)

    val SelectStepRange: Query[Step.Id, Option[TimestampInterval]] =
      sql"""
        SELECT
          MIN(c_received),
          MAX(c_received)
        FROM
          t_execution_event
        WHERE
          c_step_id = $step_id
      """.query(timestamp_interval.opt)

    val SelectVisitRange: Query[Visit.Id, Option[TimestampInterval]] =
      sql"""
        SELECT
          MIN(c_received),
          MAX(c_received)
        FROM
          t_execution_event
        WHERE
          c_visit_id = $visit_id
      """.query(timestamp_interval.opt)

    val InsertAtomEvent: Query[(Atom.Id, AtomStage), (Id,  Timestamp, Observation.Id, Visit.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_atom_stage
        )
        SELECT
          'atom' :: e_execution_event_type,
          a.c_observation_id,
          a.c_visit_id,
          $atom_id,
          $atom_stage
        FROM
          t_atom_record a
        WHERE
          a.c_atom_id = $atom_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id)
         .contramap((a, s) => (a, s, a))

    val InsertDatasetEvent: Query[(Dataset.Id, DatasetStage), (Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Step.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_dataset_id,
          c_dataset_stage
        )
        SELECT
          'dataset' :: e_execution_event_type,
          d.c_observation_id,
          d.c_visit_id,
          s.c_atom_id,
          d.c_step_id,
          $dataset_id,
          $dataset_stage
        FROM
          t_dataset d
        INNER JOIN
          t_step_record s ON s.c_step_id = d.c_step_id
        WHERE
          d.c_dataset_id = $dataset_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id *: atom_id *: step_id)
         .contramap((d, s) => (d, s, d))

    val InsertSequenceEvent: Query[(Visit.Id, SequenceCommand), (Id, Timestamp, Observation.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_sequence_command
        )
        SELECT
          'sequence' :: e_execution_event_type,
          v.c_observation_id,
          $visit_id,
          $sequence_command
        FROM
          t_visit v
        WHERE
          v.c_visit_id = $visit_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id
      """.query(execution_event_id *: core_timestamp *: observation_id)
         .contramap((v, s) => (v, s, v))

    val InsertSlewEvent: Query[(Visit.Id, SlewStage), (Id, Timestamp, Observation.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_slew_stage
        )
        SELECT
          'slew' :: e_execution_event_type,
          v.c_observation_id,
          $visit_id,
          $slew_stage
        FROM
          t_visit v
        WHERE
          v.c_visit_id = $visit_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id
      """.query(execution_event_id *: core_timestamp *: observation_id)
         .contramap((v, s) => (v, s, v))

    val InsertStepEvent: Query[(Step.Id, StepStage), (Id,  Timestamp, Observation.Id, Visit.Id, Atom.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_step_stage
        )
        SELECT
          'step' :: e_execution_event_type,
          a.c_observation_id,
          a.c_visit_id,
          s.c_atom_id,
          $step_id,
          $step_stage
        FROM
          t_step_record s
        INNER JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE
          s.c_step_id = $step_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_atom_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id *: atom_id)
         .contramap((s, t) => (s, t, s))

    val SelectMaxAtomStage: Query[Atom.Id, AtomStage] =
      sql"""
        SELECT c_atom_stage
          FROM t_execution_event
         WHERE c_atom_id = $atom_id
           AND c_atom_stage IS NOT NULL
         ORDER BY c_received DESC
         LIMIT 1
      """.query(atom_stage)

    val SelectMaxStepStage: Query[Step.Id, StepStage] =
      sql"""
        SELECT c_step_stage
          FROM t_execution_event
         WHERE c_step_id = $step_id
           AND c_step_stage IS NOT NULL
         ORDER BY c_received DESC
         LIMIT 1
      """.query(step_stage)

  }

}
