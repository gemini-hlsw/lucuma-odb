// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.syntax.show.*
import fs2.Stream
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import lucuma.core.enums.AtomStage
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.enums.StepStage
import lucuma.core.model.Client
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
import lucuma.odb.graphql.input.AddAtomEventInput
import lucuma.odb.graphql.input.AddDatasetEventInput
import lucuma.odb.graphql.input.AddSequenceEventInput
import lucuma.odb.graphql.input.AddSlewEventInput
import lucuma.odb.graphql.input.AddStepEventInput
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
    input: AddAtomEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertDatasetEvent(
    input: AddDatasetEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertSequenceEvent(
    input: AddSequenceEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertSlewEvent(
    input: AddSlewEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def insertStepEvent(
    input: AddStepEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]]

  def selectSequenceEvents(
    oid: Observation.Id
  ): Stream[F, SequenceEvent]

}

object ExecutionEventService {

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

      private val UniqueClientIdTag = "unique_client_id"

      private def duplicateEvent(c: Client.Id): OdbError.DuplicatedEvent =
        OdbError.DuplicatedEvent(c, s"An event with client id '$c' has already been added.".some)

      override def insertAtomEvent(
        input: AddAtomEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] =

        def invalidAtom: OdbError.InvalidAtom =
          OdbError.InvalidAtom(input.atomId, Some(s"Atom '${input.atomId}' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id, Visit.Id)]] =
          session
            .option(Statements.InsertAtomEvent)(input)
            .map(_.toResult(invalidAtom.asProblem))
            .recoverWith:
              case SqlState.UniqueViolation(ex) if ex.message.contains(UniqueClientIdTag) =>
                duplicateEvent(input.clientId.get).asFailureF
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidAtom.asFailureF

        (for
          e <- ResultT(insert)
          (eid, time, oid, vid) = e
          _ <- ResultT.liftF(services.sequenceService.setAtomExecutionState(input.atomId, input.atomStage))
          _ <- ResultT.liftF(services.sequenceService.abandonOngoingAtomsExcept(oid, input.atomId))
          _ <- ResultT.liftF(timeAccountingService.update(vid))
        yield AtomEvent(eid, time, oid, vid, input.clientId, input.atomId, input.atomStage)).value

      override def insertDatasetEvent(
        input: AddDatasetEventInput
      )(using xa: Transaction[F], sa: Services.ServiceAccess): F[Result[ExecutionEvent]] =

        def invalidDataset: OdbError.InvalidDataset =
          OdbError.InvalidDataset(input.datasetId, Some(s"Dataset '${input.datasetId.show}' not found"))

        val insertEvent: F[Result[(Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Step.Id)]] =
          session
            .option(Statements.InsertDatasetEvent)(input)
            .map(_.toResult(invalidDataset.asProblem))
            .recoverWith:
              case SqlState.UniqueViolation(ex) if ex.message.contains(UniqueClientIdTag) =>
                duplicateEvent(input.clientId.get).asFailureF
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidDataset.asFailureF

        // Best-effort to set the dataset time accordingly.  This can fail (leaving the timestamps
        // unchanged) if there is an end event but no start or if the end time comes before the
        // start.
        def setDatasetTime(t: Timestamp): F[Unit] =
          def setWith(f: (Dataset.Id, Timestamp) => F[Unit]): F[Unit] =
            for
              s <- xa.savepoint
              _ <- f(input.datasetId, t).recoverWith:
                case SqlState.CheckViolation(_) => xa.rollback(s).void
            yield ()

          // StartExpose signals the start of the dataset, EndWrite the end.
          input.datasetStage match
            case DatasetStage.StartExpose => setWith(services.datasetService.setStartTime)
            case DatasetStage.EndWrite    => setWith(services.datasetService.setEndTime)
            case _                        => ().pure

        (for
          e <- ResultT(insertEvent)
          (eid, time, oid, vid, aid, sid) = e
          _ <- ResultT.liftF(setDatasetTime(time))
          _ <- ResultT.liftF(timeAccountingService.update(vid))
        yield DatasetEvent(eid, time, oid, vid, input.clientId, aid, sid, input.datasetId, input.datasetStage)).value


      override def insertSequenceEvent(
        input: AddSequenceEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] =

        def invalidVisit: OdbError.InvalidVisit =
          OdbError.InvalidVisit(input.visitId, Some(s"Visit '${input.visitId}' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id)]] =
          session
            .option(Statements.InsertSequenceEvent)(input)
            .map(_.toResult(invalidVisit.asProblem))
            .recoverWith:
              case SqlState.UniqueViolation(ex) if ex.message.contains(UniqueClientIdTag) =>
                duplicateEvent(input.clientId.get).asFailureF
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidVisit.asFailureF

        (for
          e <- ResultT(insert)
          (eid, time, oid) = e
          _ <- ResultT.liftF(services.sequenceService.abandonAtomsAndStepsForObservation(oid).whenA(input.command.isTerminal))
          _ <- ResultT.liftF(timeAccountingService.update(input.visitId))
        yield SequenceEvent(eid, time, oid, input.visitId, input.clientId, input.command)).value


      override def insertSlewEvent(
        input: AddSlewEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] =

        def insert(v: Visit.Id): F[Result[(Id, Timestamp)]] =
          session
            .unique(Statements.InsertSlewEvent)(v, input)
            .map(_.success)
            .recoverWith:
              case SqlState.UniqueViolation(ex) if ex.message.contains(UniqueClientIdTag) =>
                duplicateEvent(input.clientId.get).asFailureF

        (for
          v <- ResultT(visitService.lookupOrInsert(input.observationId))
          e <- ResultT(insert(v))
          (eid, time) = e
          _ <- ResultT.liftF(timeAccountingService.update(v))
        yield SlewEvent(eid, time, input.observationId, v, input.clientId, input.slewStage)).value

      override def insertStepEvent(
        input: AddStepEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent]] =

        def invalidStep: OdbError.InvalidStep =
          OdbError.InvalidStep(input.stepId, Some(s"Step '${input.stepId}' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id, Visit.Id, Atom.Id)]] =
          session
            .option(Statements.InsertStepEvent)(input)
            .map(_.toResult(invalidStep.asProblem))
            .recoverWith:
              case SqlState.UniqueViolation(ex) if ex.message.contains(UniqueClientIdTag) =>
                duplicateEvent(input.clientId.get).asFailureF
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidStep.asFailureF

        (for
          e <- ResultT(insert)
          (eid, time, oid, vid, aid) = e
          _ <- ResultT.liftF(services.sequenceService.setAtomExecutionState(aid, AtomStage.StartAtom))
          _ <- ResultT.liftF(services.sequenceService.setStepExecutionState(input.stepId, input.stepStage, time))
          _ <- ResultT.liftF(services.sequenceService.abandonOngoingStepsExcept(oid, aid, input.stepId))
          _ <- ResultT.liftF(timeAccountingService.update(vid))
        yield StepEvent(eid, time, oid, vid, input.clientId, aid, input.stepId, input.stepStage)).value

      override def selectSequenceEvents(
        oid: Observation.Id
      ): Stream[F, SequenceEvent] =
        session.stream(Statements.SelectSequenceEvents)(oid, 256)

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

    val InsertAtomEvent: Query[AddAtomEventInput, (Id,  Timestamp, Observation.Id, Visit.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_atom_stage,
          c_client_id
        )
        SELECT
          'atom' :: e_execution_event_type,
          o.c_program_id,
          a.c_observation_id,
          a.c_visit_id,
          $atom_id,
          $atom_stage,
          ${client_id.opt}
        FROM
          t_atom_record a
        INNER JOIN
          t_observation o ON o.c_observation_id = a.c_observation_id
        WHERE
          a.c_atom_id = $atom_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id)
         .contramap(in => (in.atomId, in.atomStage, in.clientId, in.atomId))

    val InsertDatasetEvent: Query[AddDatasetEventInput, (Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Step.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_dataset_id,
          c_dataset_stage,
          c_client_id
        )
        SELECT
          'dataset' :: e_execution_event_type,
          o.c_program_id,
          d.c_observation_id,
          d.c_visit_id,
          s.c_atom_id,
          d.c_step_id,
          $dataset_id,
          $dataset_stage,
          ${client_id.opt}
        FROM
          t_dataset d
        INNER JOIN
          t_observation o ON o.c_observation_id = d.c_observation_id
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
         .contramap(in => (in.datasetId, in.datasetStage, in.clientId, in.datasetId))

    val InsertSequenceEvent: Query[AddSequenceEventInput, (Id, Timestamp, Observation.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_sequence_command,
          c_client_id
        )
        SELECT
          'sequence' :: e_execution_event_type,
          o.c_program_id,
          v.c_observation_id,
          $visit_id,
          $sequence_command,
          ${client_id.opt}
        FROM
          t_visit v
        INNER JOIN
          t_observation o ON o.c_observation_id = v.c_observation_id
        WHERE
          v.c_visit_id = $visit_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id
      """.query(execution_event_id *: core_timestamp *: observation_id)
         .contramap(in => (in.visitId, in.command, in.clientId, in.visitId))

    val InsertSlewEvent: Query[(Visit.Id, AddSlewEventInput), (Id, Timestamp)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_slew_stage,
          c_client_id
        )
        SELECT
          'slew' :: e_execution_event_type,
          o.c_program_id,
          v.c_observation_id,
          $visit_id,
          $slew_stage,
          ${client_id.opt}
        FROM
          t_visit v
        INNER JOIN
          t_observation o ON o.c_observation_id = v.c_observation_id
        WHERE
          v.c_visit_id = $visit_id
        RETURNING
          c_execution_event_id,
          c_received
      """.query(execution_event_id *: core_timestamp)
         .contramap((v, in) => (v, in.slewStage, in.clientId, v))

    val InsertStepEvent: Query[AddStepEventInput, (Id,  Timestamp, Observation.Id, Visit.Id, Atom.Id)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_step_stage,
          c_client_id
        )
        SELECT
          'step' :: e_execution_event_type,
          o.c_program_id,
          a.c_observation_id,
          a.c_visit_id,
          s.c_atom_id,
          $step_id,
          $step_stage,
          ${client_id.opt}
        FROM
          t_step_record s
        INNER JOIN
          t_atom_record a ON a.c_atom_id = s.c_atom_id
        INNER JOIN
          t_observation o ON o.c_observation_id = a.c_observation_id
        WHERE
          s.c_step_id = $step_id
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_atom_id
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id *: atom_id)
         .contramap(in => (in.stepId, in.stepStage, in.clientId, in.stepId))


    val SelectSequenceEvents: Query[Observation.Id, ExecutionEvent.SequenceEvent] =
      sql"""
        SELECT
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_client_id,
          c_sequence_command
        FROM
          t_execution_event
        WHERE
          c_observation_id = $observation_id AND
          c_sequence_command IS NOT NULL
        ORDER BY
          c_received
      """.query((
        execution_event_id *:
        core_timestamp     *:
        observation_id     *:
        visit_id           *:
        client_id.opt      *:
        sequence_command
      ).to[ExecutionEvent.SequenceEvent])

  }

}
