// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.applicativeError.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.functor.*
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import Services.Syntax.*


trait ExecutionEventService[F[_]] {

  def insertSequenceEvent(
    visitId: Visit.Id,
    command: SequenceCommand
  )(using Transaction[F]): F[ExecutionEventService.InsertEventResponse]

  def insertStepEvent(
    stepId:       Step.Id,
    sequenceType: SequenceType,
    stepStage:    StepStage
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

    case class VisitNotFound(
      id: Visit.Id
    ) extends InsertEventResponse

    case class Success(
      eid: ExecutionEvent.Id
    ) extends InsertEventResponse
  }

  def instantiate[F[_]: Concurrent](using Services[F]): ExecutionEventService[F] =
    new ExecutionEventService[F] with ExecutionUserCheck {

      override def insertSequenceEvent(
        visitId: Visit.Id,
        command: SequenceCommand
      )(using Transaction[F]): F[InsertEventResponse] = {

        import InsertEventResponse.*

        val insert: F[Either[VisitNotFound, ExecutionEvent.Id]] =
          session
            .unique(Statements.InsertSequenceEvent)(visitId, command)
            .map(_.asRight)
            .recover {
              case SqlState.ForeignKeyViolation(_) => VisitNotFound(visitId).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          e <- EitherT(insert).leftWiden[InsertEventResponse]
        } yield Success(e)).merge
      }

      override def insertStepEvent(
        stepId:       Step.Id,
        sequenceType: SequenceType,
        stepStage:    StepStage
      )(using Transaction[F]): F[InsertEventResponse] = {

        import InsertEventResponse.*

        val insert: F[Either[StepNotFound, ExecutionEvent.Id]] =
          session
            .option(Statements.InsertStepEvent)(stepId, sequenceType, stepStage, stepId)
            .map(_.toRight(StepNotFound(stepId)))
            .recover {
              case SqlState.ForeignKeyViolation(_) => StepNotFound(stepId).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          e <- EitherT(insert).leftWiden[InsertEventResponse]
        } yield Success(e)).merge
      }
    }

  object Statements {

    val InsertSequenceEvent: Query[(Visit.Id, SequenceCommand), ExecutionEvent.Id] =
      sql"""
        INSERT INTO t_sequence_event (
          c_visit_id,
          c_sequence_command
        )
        SELECT
          $visit_id,
          $sequence_command
        RETURNING
          c_execution_event_id
      """.query(execution_event_id)

    val InsertStepEvent: Query[(Step.Id, SequenceType, StepStage, Step.Id), ExecutionEvent.Id] =
      sql"""
        INSERT INTO t_step_event (
          c_visit_id,
          c_step_id,
          c_sequence_type,
          c_step_stage
        )
        SELECT
          c_visit_id,
          $step_id,
          $sequence_type,
          $step_stage
        FROM
          t_step
        WHERE
          t_step.c_step_id = $step_id

        RETURNING
          c_execution_event_id
      """.query(execution_event_id)
  }

}
