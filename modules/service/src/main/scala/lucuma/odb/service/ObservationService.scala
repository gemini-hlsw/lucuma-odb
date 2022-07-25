// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.GuestRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole._
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.Tag
import lucuma.odb.util.Codecs._
import skunk._
import skunk.implicits._
import lucuma.odb.graphql.snippet.input.ObservationPropertiesInput

trait ObservationService[F[_]] {
  import ObservationService._

  def insertObservation(
    programId:   Program.Id,
    SET:         ObservationPropertiesInput,
  ): F[InsertObservationResponse]

}


object ObservationService {

  sealed trait InsertObservationResponse
  object InsertObservationResponse {
    case class NotAuthorized(user: User)   extends InsertObservationResponse
    case class Success(id: Observation.Id) extends InsertObservationResponse
  }
  import InsertObservationResponse._

  def fromUserAndSession[F[_]: MonadCancelThrow](user: User, session: Session[F]): ObservationService[F] =
    new ObservationService[F] {

      def insertObservation(
        programId:   Program.Id,
        SET:         ObservationPropertiesInput,
      ): F[InsertObservationResponse] = {
        val af = Statements.insertObservationAs(user, programId, SET.subtitle, SET.existence.getOrElse(Existence.Default), SET.status.getOrElse(ObsStatus.Default), SET.activeStatus.getOrElse(ObsActiveStatus.Default))
        session.prepare(af.fragment.query(observation_id)).use { pq =>
          pq.option(af.argument).map {
            case Some(oid) => Success(oid)
            case None      => NotAuthorized(user)
          }
        }
      }

    }

  object Statements {
    import ProgramService.Statements.{ existsUserAsPi, existsUserAsCoi, existsAllocationForPartner }

    def insertObservationAs(
      user: User,
      programId:   Program.Id,
      name:        Option[NonEmptyString],
      existence:   Existence,
      status:      ObsStatus,
      activeState: ObsActiveStatus
    ): AppliedFragment = {

      val insert: AppliedFragment =
        InsertObservation.apply(programId ~ name ~ existence ~ status ~ activeState)

      val where: AppliedFragment =
        user.role match {
          case GuestRole       => void"WHERE " |+| existsUserAsPi(programId, user.id)
          case Pi(_)           => void"WHERE " |+| existsUserAsPi(programId, user.id) |+| void" OR " |+| existsUserAsCoi(programId, user.id)
          case Ngo(_, partner) => void"WHERE " |+| existsAllocationForPartner(programId, Tag(partner.tag))
          case ServiceRole(_)  |
               Admin(_)        |
               Staff(_)        => AppliedFragment.empty
        }

      val returning: AppliedFragment =
        void"RETURNING c_observation_id"

      // done!
      insert |+| where |+| returning

    }

    val InsertObservation: Fragment[Program.Id ~ Option[NonEmptyString] ~ Existence ~ ObsStatus ~ ObsActiveStatus] =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_name,
          c_existence,
          c_status,
          c_active_status
        )
        SELECT
          $program_id,
          ${text_nonempty.opt},
          $existence,
          $obs_status,
          $obs_active_status
      """

  }

}