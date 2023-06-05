// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.EitherT
import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.Instrument
import lucuma.core.model.Access.Admin
import lucuma.core.model.Access.Service
import lucuma.core.model.Access.Staff
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.implicits.*

import java.util.UUID

import Services.Syntax.*

trait VisitService[F[_]] {

  def insertGmosNorth(
    observationId: Observation.Id,
    static:        GmosNorth
  )(using Transaction[F]): F[VisitService.InsertVisitResponse]

  def insertGmosSouth(
    observationId: Observation.Id,
    static:        GmosSouth
  )(using Transaction[F]): F[VisitService.InsertVisitResponse]

}

object VisitService {

  sealed trait InsertVisitResponse extends Product with Serializable
  object InsertVisitResponse {

    case class NotAuthorized(
      user: User
    ) extends InsertVisitResponse

    case class ObservationNotFound(
      id:         Observation.Id,
      instrument: Instrument
    ) extends InsertVisitResponse

    case class Success(
      vid: Visit.Id
    )  extends InsertVisitResponse

  }

  def instantiate[F[_]: Concurrent: UUIDGen](using Services[F]): VisitService[F] =
    new VisitService[F] with ExecutionUserCheck {

      private def insert(
        observationId: Observation.Id,
        instrument:    Instrument,
        insertStatic:  Option[Visit.Id] => F[Long]
      ): F[InsertVisitResponse] = {
        import InsertVisitResponse.*

        def insertVisit(v: Visit.Id): F[Either[ObservationNotFound, Unit]] =
          session
            .execute(Statements.InsertVisit)(v, observationId, instrument)
            .as(().asRight)
            .recover {
              case SqlState.ForeignKeyViolation(_) =>
                ObservationNotFound(observationId, instrument).asLeft
            }

        (for {
          _ <- EitherT.fromEither(checkUser(NotAuthorized.apply))
          v <- EitherT.right(UUIDGen[F].randomUUID).map(Visit.Id.fromUuid)
          _ <- EitherT(insertVisit(v))
          _ <- EitherT.right(insertStatic(v.some).void)
        } yield Success(v)).merge
      }

      override def insertGmosNorth(
        observationId: Observation.Id,
        static:        GmosNorth
      )(using Transaction[F]): F[InsertVisitResponse] =
        insert(observationId, Instrument.GmosNorth, gmosSequenceService.insertGmosNorthStatic(observationId, _, static))

      override def insertGmosSouth(
        observationId: Observation.Id,
        static:        GmosSouth
      )(using Transaction[F]): F[InsertVisitResponse] =
        insert(observationId, Instrument.GmosSouth, gmosSequenceService.insertGmosSouthStatic(observationId, _, static))

    }

  object Statements {

    val InsertVisit: Command[(Visit.Id, Observation.Id, Instrument)] =
      sql"""
        INSERT INTO t_visit (
          c_visit_id,
          c_observation_id,
          c_instrument
        )
        SELECT
          $visit_id,
          $observation_id,
          $instrument
      """.command

  }
}