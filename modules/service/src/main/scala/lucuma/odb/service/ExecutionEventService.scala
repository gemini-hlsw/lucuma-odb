// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.show.*
import grackle.Result
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
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*

trait ExecutionEventService[F[_]] {

  def atomRange(
    atomId: Atom.Id
  )(using Transaction[F]): F[Option[TimestampInterval]]

  def stepRange(
    visitId: Step.Id
  )(using Transaction[F]): F[Option[TimestampInterval]]

  def visitRange(
    visitId: Visit.Id
  )(using Transaction[F]): F[Option[TimestampInterval]]

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

}

object ExecutionEventService {

  sealed trait InsertEventResponse extends Product with Serializable {

    def asSuccess: Option[InsertEventResponse.Success] =
      this match {
        case s@InsertEventResponse.Success(_) => s.some
        case _                                => none
      }

  }

  object InsertEventResponse {

    case class StepNotFound(
      id: Step.Id
    ) extends InsertEventResponse

    case class DatasetNotFound(
      id: Dataset.Id
    ) extends InsertEventResponse

    case class VisitNotFound(
      id: Visit.Id
    ) extends InsertEventResponse

    case class Success(
      event: ExecutionEvent
    ) extends InsertEventResponse
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

      override def insertDatasetEvent(
        datasetId:    Dataset.Id,
        datasetStage: DatasetStage
      )(using xa: Transaction[F], sa: Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        import InsertEventResponse.*

        val insertEvent: F[Either[DatasetNotFound, (Id, Timestamp, Observation.Id, Visit.Id, Step.Id)]] =
          session
            .option(Statements.InsertDatasetEvent)(datasetId, datasetStage, datasetId)
            .map(_.toRight(DatasetNotFound(datasetId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => DatasetNotFound(datasetId).asLeft
            }

        // Best-effort to set the dataset time accordingly.  This can fail (leaving the timestamps
        // unchanged) if there is an end event but no start or if the end time comes before the
        // start.9
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
          e <- EitherT(insertEvent).leftWiden[InsertEventResponse]
          (eid, time, oid, vid, sid) = e
          _ <- EitherT.liftF(setDatasetTime(time))
        } yield Success(DatasetEvent(eid, time, oid, vid, sid, datasetId, datasetStage))).merge

      } .map(executionEventResponseToResult)
        .flatTap(_.traverse(e => timeAccountingService.update(e.visitId)))

      private def executionEventResponseToResult(r: InsertEventResponse): Result[ExecutionEvent] =
        r match
          case InsertEventResponse.DatasetNotFound(id) => OdbError.InvalidDataset(id, Some(s"Dataset '${id.show}' not found")).asFailure
          case InsertEventResponse.StepNotFound(id)    => OdbError.InvalidStep(id, Some(s"Step '$id' not found")).asFailure
          case InsertEventResponse.VisitNotFound(id)   => OdbError.InvalidVisit(id, Some(s"Visit '$id' not found")).asFailure
          case InsertEventResponse.Success(e)          => Result(e)

      override def insertSequenceEvent(
        visitId: Visit.Id,
        command: SequenceCommand
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        import InsertEventResponse.*

        val insert: F[Either[VisitNotFound, (Id, Timestamp, Observation.Id)]] =
          session
            .option(Statements.InsertSequenceEvent)(visitId, command, visitId)
            .map(_.toRight(VisitNotFound(visitId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => VisitNotFound(visitId).asLeft
            }

        (for {
          e <- EitherT(insert).leftWiden[InsertEventResponse]
          (eid, time, oid) = e
        } yield Success(SequenceEvent(eid, time, oid, visitId, command))).merge
      } .map(executionEventResponseToResult)
        .flatTap(_.traverse(e => timeAccountingService.update(e.visitId)))

      override def insertSlewEvent(
        visitId:   Visit.Id,
        slewStage: SlewStage
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        import InsertEventResponse.*

        val insert: F[Either[VisitNotFound, (Id, Timestamp, Observation.Id)]] =
          session
            .option(Statements.InsertSlewEvent)(visitId, slewStage, visitId)
            .map(_.toRight(VisitNotFound(visitId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => VisitNotFound(visitId).asLeft
            }

        (for {
          e <- EitherT(insert).leftWiden[InsertEventResponse]
          (eid, time, oid) = e
        } yield Success(SlewEvent(eid, time, oid, visitId, slewStage))).merge
      } .map(executionEventResponseToResult)
        .flatTap(_.traverse(e => timeAccountingService.update(e.visitId)))

      override def insertStepEvent(
        stepId:       Step.Id,
        stepStage:    StepStage
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] = {

        import InsertEventResponse.*

        val insert: F[Either[StepNotFound, (Id, Timestamp, Observation.Id, Visit.Id)]] =
          session
            .option(Statements.InsertStepEvent)(stepId, stepStage, stepId)
            .map(_.toRight(StepNotFound(stepId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => StepNotFound(stepId).asLeft
            }

        (for {
          e <- EitherT(insert).leftWiden[InsertEventResponse]
          (eid, time, oid, vid) = e
          _ <- EitherT.liftF(
              // N.B. This is probably too simplistic. We'll need to examine
              // datasets as well I believe.
              services
                .sequenceService
                .setStepCompleted(stepId, Option.when(stepStage === StepStage.EndStep)(time))
          )
        } yield Success(StepEvent(eid, time, oid, vid, stepId, stepStage))).merge
      } .map(executionEventResponseToResult)
        .flatTap(_.traverse(e => timeAccountingService.update(e.visitId)))

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

    val InsertDatasetEvent: Query[(Dataset.Id, DatasetStage, Dataset.Id), (Id, Timestamp, Observation.Id, Visit.Id, Step.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_step_id,
          c_dataset_id,
          c_dataset_stage
        )
        SELECT
          'dataset' :: e_execution_event_type,
          d.c_observation_id,
          d.c_visit_id,
          d.c_step_id,
          $dataset_id,
          $dataset_stage
        FROM
          t_dataset d
        WHERE
          d.c_dataset_id = $dataset_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_step_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id *: step_id)

    val InsertSequenceEvent: Query[(Visit.Id, SequenceCommand, Visit.Id), (Id, Timestamp, Observation.Id)] =
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

    val InsertSlewEvent: Query[(Visit.Id, SlewStage, Visit.Id), (Id, Timestamp, Observation.Id)] =
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

    val InsertStepEvent: Query[(Step.Id, StepStage, Step.Id), (Id,  Timestamp, Observation.Id, Visit.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_observation_id,
          c_visit_id,
          c_step_id,
          c_step_stage
        )
        SELECT
          'step' :: e_execution_event_type,
          a.c_observation_id,
          a.c_visit_id,
          $step_id,
          $step_stage
        FROM
          t_step_record s
        LEFT JOIN t_atom_record a ON a.c_atom_id = s.c_atom_id
        WHERE
          s.c_step_id = $step_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id)
  }

}
