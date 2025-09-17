// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
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
import skunk.codec.boolean.bool
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
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]]

  def insertDatasetEvent(
    input: AddDatasetEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]]

  def insertSequenceEvent(
    input: AddSequenceEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]]

  def insertSlewEvent(
    input: AddSlewEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]]

  def insertStepEvent(
    input: AddStepEventInput
  )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]]

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

      override def insertAtomEvent(
        input: AddAtomEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]] =

        def invalidAtom: OdbError.InvalidAtom =
          OdbError.InvalidAtom(input.atomId, Some(s"Atom '${input.atomId}' not found"))

        val insert: F[Result[(Id, Observation.Id, Visit.Id, Boolean)]] =
          session
            .option(Statements.InsertAtomEvent)(input)
            .map(_.toResult(invalidAtom.asProblem))
            .recoverWith:
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidAtom.asFailureF

        ResultT(insert)
          .flatMap: (eid, oid, vid, wasInserted) =>
            if wasInserted then
              ResultT.liftF:
                for
                  _ <- services.sequenceService.setAtomExecutionState(input.atomId, input.atomStage)
                  _ <- services.sequenceService.abandonOngoingStepsExcept(oid, input.atomId, none)
                  _ <- timeAccountingService.update(vid)
                yield eid
            else
              ResultT.pure(eid)
          .value

      override def insertDatasetEvent(
        input: AddDatasetEventInput
      )(using xa: Transaction[F], sa: Services.ServiceAccess): F[Result[ExecutionEvent.Id]] =

        def invalidDataset: OdbError.InvalidDataset =
          OdbError.InvalidDataset(input.datasetId, Some(s"Dataset '${input.datasetId.show}' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Step.Id, Boolean)]] =
          session
            .option(Statements.InsertDatasetEvent)(input)
            .map(_.toResult(invalidDataset.asProblem))
            .recoverWith:
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

        ResultT(insert)
          .flatMap: (eid, time, oid, vid, aid, sid, wasInserted) =>
            if wasInserted then
              ResultT.liftF:
                services.sequenceService.abandonOngoingStepsExcept(oid, aid, sid.some) *>
                setDatasetTime(time)                                                   *>
                timeAccountingService.update(vid).as(eid)
            else
              ResultT.pure(eid)
          .value

      override def insertSequenceEvent(
        input: AddSequenceEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]] =

        def invalidVisit: OdbError.InvalidVisit =
          OdbError.InvalidVisit(input.visitId, Some(s"Visit '${input.visitId}' not found"))

        val insert: F[Result[(Id, Observation.Id, Boolean)]] =
          session
            .option(Statements.InsertSequenceEvent)(input)
            .map(_.toResult(invalidVisit.asProblem))
            .recoverWith:
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidVisit.asFailureF

        ResultT(insert)
          .flatMap: (eid, oid, wasInserted) =>
            if wasInserted then
              ResultT.liftF:
                for
                  _ <- services.sequenceService.abandonOngoingSteps(oid).whenA(input.command.isTerminal)
                  _ <- timeAccountingService.update(input.visitId)
                yield eid
            else
              ResultT.pure(eid)
          .value

      override def insertSlewEvent(
        input: AddSlewEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]] =

        def insert(v: Visit.Id): F[Result[(Id, Boolean)]] =
          session
            .unique(Statements.InsertSlewEvent)(v, input)
            .map(_.success)

        (for
          v <- ResultT(visitService.lookupOrInsert(input.observationId, none))
          e <- ResultT(insert(v))
          (eid, wasInserted) = e
          _ <- ResultT.liftF(timeAccountingService.update(v)).whenA(wasInserted)
        yield eid).value

      override def insertStepEvent(
        input: AddStepEventInput
      )(using Transaction[F], Services.ServiceAccess): F[Result[ExecutionEvent.Id]] =

        def invalidStep: OdbError.InvalidStep =
          OdbError.InvalidStep(input.stepId, Some(s"Step '${input.stepId}' not found"))

        val insert: F[Result[(Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Boolean)]] =
          session
            .option(Statements.InsertStepEvent)(input)
            .map(_.toResult(invalidStep.asProblem))
            .recoverWith:
              case SqlState.ForeignKeyViolation(_)                                        =>
                invalidStep.asFailureF

        ResultT(insert)
          .flatMap: (eid, time, oid, vid, aid, wasInserted) =>
            if wasInserted then
              for
                _ <- ResultT.liftF(services.sequenceService.setAtomExecutionState(aid, AtomStage.StartAtom))
                _ <- ResultT.liftF(services.sequenceService.setStepExecutionState(input.stepId, input.stepStage, time))
                _ <- ResultT.liftF(services.sequenceService.abandonOngoingStepsExcept(oid, aid, input.stepId.some))
                _ <- ResultT.liftF(timeAccountingService.update(vid))
              yield eid
            else
              ResultT.pure(eid)
          .value

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

    val InsertAtomEvent: Query[AddAtomEventInput, (Id, Observation.Id, Visit.Id, Boolean)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_atom_stage,
          c_idempotency_key
        )
        SELECT
          'atom' :: e_execution_event_type,
          o.c_program_id,
          a.c_observation_id,
          a.c_visit_id,
          $atom_id,
          $atom_stage,
          ${idempotency_key.opt}
        FROM
          t_atom_record a
        INNER JOIN
          t_observation o ON o.c_observation_id = a.c_observation_id
        WHERE
          a.c_atom_id = $atom_id
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_execution_event_id,
          c_observation_id,
          c_visit_id,
          xmax = 0 AS inserted
      """.query(execution_event_id *: observation_id *: visit_id *: bool)
         .contramap(in => (in.atomId, in.atomStage, in.idempotencyKey, in.atomId))

    val InsertDatasetEvent: Query[AddDatasetEventInput, (Id, Timestamp, Observation.Id, Visit.Id, Atom.Id, Step.Id, Boolean)] =
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
          c_idempotency_key
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
          ${idempotency_key.opt}
        FROM
          t_dataset d
        INNER JOIN
          t_observation o ON o.c_observation_id = d.c_observation_id
        INNER JOIN
          t_step_record s ON s.c_step_id = d.c_step_id
        WHERE
          d.c_dataset_id = $dataset_id
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          xmax = 0 AS inserted
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id *: atom_id *: step_id *: bool)
         .contramap(in => (in.datasetId, in.datasetStage, in.idempotencyKey, in.datasetId))

    val InsertSequenceEvent: Query[AddSequenceEventInput, (Id, Observation.Id, Boolean)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_sequence_command,
          c_idempotency_key
        )
        SELECT
          'sequence' :: e_execution_event_type,
          o.c_program_id,
          v.c_observation_id,
          $visit_id,
          $sequence_command,
          ${idempotency_key.opt}
        FROM
          t_visit v
        INNER JOIN
          t_observation o ON o.c_observation_id = v.c_observation_id
        WHERE
          v.c_visit_id = $visit_id
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_execution_event_id,
          c_observation_id,
          xmax = 0 AS inserted
      """.query(execution_event_id *: observation_id *: bool)
         .contramap(in => (in.visitId, in.command, in.idempotencyKey, in.visitId))

    val InsertSlewEvent: Query[(Visit.Id, AddSlewEventInput), (Id, Boolean)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_slew_stage,
          c_idempotency_key
        )
        SELECT
          'slew' :: e_execution_event_type,
          o.c_program_id,
          v.c_observation_id,
          $visit_id,
          $slew_stage,
          ${idempotency_key.opt}
        FROM
          t_visit v
        INNER JOIN
          t_observation o ON o.c_observation_id = v.c_observation_id
        WHERE
          v.c_visit_id = $visit_id
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_execution_event_id,
          xmax = 0 AS inserted
      """.query(execution_event_id *: bool)
         .contramap((v, in) => (v, in.slewStage, in.idempotencyKey, v))

    val InsertStepEvent: Query[AddStepEventInput, (Id,  Timestamp, Observation.Id, Visit.Id, Atom.Id, Boolean)] =
      sql"""
        INSERT INTO t_execution_event (
          c_event_type,
          c_program_id,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          c_step_id,
          c_step_stage,
          c_idempotency_key
        )
        SELECT
          'step' :: e_execution_event_type,
          o.c_program_id,
          a.c_observation_id,
          a.c_visit_id,
          s.c_atom_id,
          $step_id,
          $step_stage,
          ${idempotency_key.opt}
        FROM
          t_step_record s
        INNER JOIN
          t_atom_record a ON a.c_atom_id = s.c_atom_id
        INNER JOIN
          t_observation o ON o.c_observation_id = a.c_observation_id
        WHERE
          s.c_step_id = $step_id
        ON CONFLICT (c_idempotency_key) DO UPDATE
          SET c_idempotency_key = EXCLUDED.c_idempotency_key
        RETURNING
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_atom_id,
          xmax = 0 AS inserted
      """.query(execution_event_id *: core_timestamp *: observation_id *: visit_id *: atom_id *: bool)
         .contramap(in => (in.stepId, in.stepStage, in.idempotencyKey, in.stepId))

    val SelectSequenceEvents: Query[Observation.Id, ExecutionEvent.SequenceEvent] =
      sql"""
        SELECT
          c_execution_event_id,
          c_received,
          c_observation_id,
          c_visit_id,
          c_idempotency_key,
          c_sequence_command
        FROM
          t_execution_event
        WHERE
          c_observation_id = $observation_id AND
          c_sequence_command IS NOT NULL
        ORDER BY
          c_received
      """.query((
        execution_event_id  *:
        core_timestamp      *:
        observation_id      *:
        visit_id            *:
        idempotency_key.opt *:
        sequence_command
      ).to[ExecutionEvent.SequenceEvent])

  }

}
