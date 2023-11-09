// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.apply.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import fs2.Stream
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent.Id
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.odb.data.ExecutionEvent
import lucuma.odb.data.ExecutionEventType
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*


trait ExecutionEventService[F[_]] {

  /**
   * Streams execution events that fall in the range during which the visit
   * was active.  This will include events for other visits that fall in
   * time range as well.
   */
  def streamEvents(
    visitId: Visit.Id
  ): Stream[F, ExecutionEvent]

  def insertDatasetEvent(
    datasetId:    Dataset.Id,
    datasetStage: DatasetStage
  )(using Transaction[F]): F[ExecutionEventService.InsertEventResponse]

  def insertSequenceEvent(
    visitId: Visit.Id,
    command: SequenceCommand
  )(using Transaction[F]): F[ExecutionEventService.InsertEventResponse]

  def insertStepEvent(
    stepId:    Step.Id,
    stepStage: StepStage
  )(using Transaction[F]): F[ExecutionEventService.InsertEventResponse]

}

object ExecutionEventService {

  sealed trait InsertEventResponse extends Product with Serializable

  object InsertEventResponse {
    case class NotAuthorized(
      user: User
    ) extends InsertEventResponse

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
      eid:  Id,
      time: Timestamp
    ) extends InsertEventResponse
  }

  def instantiate[F[_]: Concurrent](using Services[F]): ExecutionEventService[F] =
    new ExecutionEventService[F] with ExecutionUserCheck {

      override def streamEvents(
        visitId: Visit.Id
      ): Stream[F, ExecutionEvent] =
        Stream
          .eval(session.unique(Statements.SelectVisitRange)(visitId))
          .flatMap {
            case None        => Stream.empty
            case Some(range) => session.stream(Statements.SelectEventsInRange)(range, 1024)
          }

      override def insertDatasetEvent(
        datasetId:    Dataset.Id,
        datasetStage: DatasetStage
      )(using xa: Transaction[F]): F[ExecutionEventService.InsertEventResponse] = {

        import InsertEventResponse.*

        val insertEvent: F[Either[DatasetNotFound, (Id, Timestamp)]] =
          session
            .option(Statements.InsertDatasetEvent)(datasetId, datasetStage, datasetId)
            .map(_.toRight(DatasetNotFound(datasetId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => DatasetNotFound(datasetId).asLeft
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

          // StartObserve signals the start of the dataset, EndWrite the end.
          datasetStage match {
            case DatasetStage.StartObserve => setWith(services.datasetService.setStartTime)
            case DatasetStage.EndWrite     => setWith(services.datasetService.setEndTime)
            case _                         => ().pure
          }
        }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          e <- EitherT(insertEvent).leftWiden[InsertEventResponse]
          (eid, time) = e
          _ <- EitherT.liftF(setDatasetTime(time))
        } yield Success(eid, time)).merge

      }

      override def insertSequenceEvent(
        visitId: Visit.Id,
        command: SequenceCommand
      )(using Transaction[F]): F[InsertEventResponse] = {

        import InsertEventResponse.*

        val insert: F[Either[VisitNotFound, (Id, Timestamp)]] =
          session
            .option(Statements.InsertSequenceEvent)(visitId, command, visitId)
            .map(_.toRight(VisitNotFound(visitId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => VisitNotFound(visitId).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          e <- EitherT(insert).leftWiden[InsertEventResponse]
          (eid, time) = e
        } yield Success(eid, time)).merge
      }

      override def insertStepEvent(
        stepId:       Step.Id,
        stepStage:    StepStage
      )(using Transaction[F]): F[InsertEventResponse] = {

        import InsertEventResponse.*

        val insert: F[Either[StepNotFound, (Id, Timestamp)]] =
          session
            .option(Statements.InsertStepEvent)(stepId, stepStage, stepId)
            .map(_.toRight(StepNotFound(stepId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => StepNotFound(stepId).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          e <- EitherT(insert).leftWiden[InsertEventResponse]
          (eid, time) = e
          _ <- EitherT.liftF(
              // N.B. This is probably too simplistic. We'll need to examine
              // datasets as well I believe.
              services
                .sequenceService
                .setStepCompleted(stepId, Option.when(stepStage === StepStage.EndStep)(time))
          )
        } yield Success(eid, time)).merge
      }
    }

  object Statements {

    val SelectVisitRange: Query[Visit.Id, Option[(Timestamp, Timestamp)]] =
      sql"""
        SELECT
          MIN(c_received),
          MAX(c_received)
        FROM
          t_execution_event
        WHERE
          c_visit_id = $visit_id
      """.query(
        (core_timestamp.opt *: core_timestamp.opt).map(_.tupled)
      )

    private val execution_event: Codec[ExecutionEvent] =
      (
        execution_event_type *:
        core_timestamp       *:
        observation_id       *:
        visit_id             *:
        step_id.opt          *:
        dataset_id.opt       *:
        sequence_command.opt *:
        step_stage.opt       *:
        dataset_stage.opt
      ).eimap[ExecutionEvent] { case (eventType, rec, oid, vid, sid, did, scmd, ss, ds) =>

        eventType match {

          case ExecutionEventType.Sequence =>
            scmd.toRight("Cannot decode sequence execution event.  Missing c_sequence_command.").map { c =>
              ExecutionEvent.SequenceEvent(rec, oid, vid, c)
            }

          case ExecutionEventType.Step =>
            (sid, ss).mapN { (stepId, stepStage) =>
              ExecutionEvent.StepEvent(rec, oid, vid, stepId, stepStage)
            }.toRight(s"Cannot decode step execution event. Missing c_step_id ($sid) and/or c_step_stage ($ss) value")

          case ExecutionEventType.Dataset =>
            (sid, did, ds).mapN { (stepId, datasetId, datasetStage) =>
              ExecutionEvent.DatasetEvent(rec, oid, vid, stepId, datasetId, datasetStage)
            }.toRight(s"Cannot decode dataset execution event. Missing c_step_id ($sid) and/or c_dataset_id ($did) and/or c_dataset_stage ($ds)")

        }

      } { event =>
        event.fold(
          e => (e.eventType, e.received, e.observationId, e.visitId, none, none, e.command.some, none, none),
          e => (e.eventType, e.received, e.observationId, e.visitId, e.stepId.some, none, none, e.stage.some, none),
          e => (e.eventType, e.received, e.observationId, e.visitId, e.stepId.some, e.datasetId.some, none, none, e.stage.some)
        )
      }

    val SelectEventsInRange: Query[(Timestamp, Timestamp), ExecutionEvent] =
      sql"""
        SELECT
          c_event_type,
          c_received,
          c_observation_id,
          c_visit_id,
          c_step_id,
          c_dataset_id,
          c_sequence_command,
          c_step_stage,
          c_dataset_stage
        FROM
          t_execution_event
        WHERE
          c_received BETWEEN $core_timestamp AND $core_timestamp
      """.query(execution_event)

    val InsertDatasetEvent: Query[(Dataset.Id, DatasetStage, Dataset.Id), (Id, Timestamp)] =
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
          c_received
      """.query(execution_event_id *: core_timestamp)

    val InsertSequenceEvent: Query[(Visit.Id, SequenceCommand, Visit.Id), (Id, Timestamp)] =
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
          c_received
      """.query(execution_event_id *: core_timestamp)

    val InsertStepEvent: Query[(Step.Id, StepStage, Step.Id), (Id, Timestamp)] =
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
          c_received
      """.query(execution_event_id *: core_timestamp)
  }

}
