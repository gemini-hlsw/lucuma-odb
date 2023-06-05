// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.applicativeError.*
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import lucuma.core.enums.SequenceCommand
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import java.util.UUID

import Services.Syntax.*


trait ExecutionEventService[F[_]] {

  def insertExecutionEvent(
    visitId: Visit.Id,
    command: SequenceCommand
  )(using Transaction[F]): F[ExecutionEventService.InsertEventResponse]

}

object ExecutionEventService {

  sealed trait InsertEventResponse extends Product with Serializable

  object InsertEventResponse {
    case class NotAuthorized(
      user: User
    ) extends InsertEventResponse

    case class VisitNotFound(
      id: Visit.Id
    ) extends InsertEventResponse

    case class Success(
      eid: ExecutionEvent.Id
    ) extends InsertEventResponse
  }

  def instantiate[F[_]: Concurrent: UUIDGen](using Services[F]): ExecutionEventService[F] =
    new ExecutionEventService[F] with ExecutionUserCheck {

      override def insertExecutionEvent(
        visitId: Visit.Id,
        command: SequenceCommand
      )(using Transaction[F]): F[InsertEventResponse] = {

        import InsertEventResponse.*

        def insert(e: ExecutionEvent.Id): F[Either[VisitNotFound, Unit]] =
          session
            .execute(Statements.InsertSequenceEvent)(e, visitId, command)
            .as(().asRight)
            .recover {
              case SqlState.ForeignKeyViolation(_) => VisitNotFound(visitId).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          e <- EitherT.right(UUIDGen[F].randomUUID.map(ExecutionEvent.Id.fromUuid))
          _ <- EitherT(insert(e)).leftWiden[InsertEventResponse]
        } yield Success(e)).merge
      }

    }

  object Statements {

    val InsertSequenceEvent: Command[(ExecutionEvent.Id, Visit.Id, SequenceCommand)] =
      sql"""
        INSERT INTO t_sequence_event (
          c_execution_event_id,
          c_visit_id,
          c_sequence_command
        )
        SELECT
          $execution_event_id,
          $visit_id,
          $sequence_command
      """.command

  }

}
