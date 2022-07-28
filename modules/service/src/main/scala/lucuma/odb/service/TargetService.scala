// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import lucuma.core.model.EphemerisKey
import lucuma.core.model.GuestUser
import lucuma.core.model.Program
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardRole.Admin
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Staff
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.Tag
import lucuma.odb.graphql.snippet.input.TargetPropertiesInput
import lucuma.odb.graphql.snippet.input.SiderealInput
import lucuma.odb.util.Codecs._
import skunk.AppliedFragment
import skunk.Session
import skunk.circe.codec.all._
import skunk.codec.all._
import skunk.implicits._
import lucuma.odb.graphql.instances.SourceProfileEncoder

trait TargetService[F[_]] {
  import TargetService.CreateTargetResponse
  def createTarget(pid: Program.Id, input: TargetPropertiesInput): F[CreateTargetResponse]
}

object TargetService {

  sealed trait CreateTargetResponse
  object CreateTargetResponse {
    case class NotAuthorized(user: User)        extends CreateTargetResponse
    case class ProgramNotFound(pid: Program.Id) extends CreateTargetResponse
    case class Success(id: Target.Id)           extends CreateTargetResponse
  }
  import CreateTargetResponse._

    def fromSession[F[_]: MonadCancelThrow](s: Session[F], u: User): TargetService[F] =
      new TargetService[F] {
        import Statements._

        override def createTarget(pid: Program.Id, input: TargetPropertiesInput): F[CreateTargetResponse] = {
          val insert: AppliedFragment =
            input.tracking match {
              case Left(s) => insertSiderealFragment(pid, input.name, s, SourceProfileEncoder.EncoderSourceProfile(input.sourceProfile))
              case Right(n) => insertNonsiderealFragment(pid, input.name, n, SourceProfileEncoder.EncoderSourceProfile(input.sourceProfile))
            }
          val where = whereFragment(pid, u)
          val appl  = insert |+| void" " |+| where
          s.prepare(appl.fragment.query(target_id)).use { ps =>
            ps.option(appl.argument).map {
              case Some(tid) => Success(tid)
              case None      => NotAuthorized(u)
            } // todo: catch key violation to indicate ProgramNotFound
          }
        }

      }


  object Statements {

    import ProgramService.Statements.{ existsUserAsPi, existsUserAsCoi, existsAllocationForPartner }

    def insertSiderealFragment(
      pid:  Program.Id,
      name: NonEmptyString,
      si:   SiderealInput,
      sourceProfile: Json
    ): AppliedFragment = {
      sql"""
        insert into t_target (
          c_program_id,
          c_name,
          c_type,
          c_sid_ra,
          c_sid_dec,
          c_sid_epoch,
          c_sid_pm_ra,
          c_sid_pm_dec,
          c_sid_rv,
          c_sid_parallax,
          c_sid_catalog_name,
          c_sid_catalog_id,
          c_sid_catalog_object_type,
          c_source_profile
        )
        select
          $program_id,
          $text_nonempty,
          'sidereal',
          ${right_ascension.opt},
          ${declination.opt},
          ${epoch.opt},
          ${int8.opt},
          ${int8.opt},
          ${radial_velocity.opt},
          ${parallax.opt},
          ${catalog_name.opt},
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          $json
      """.apply(
        pid ~
        name ~
        si.ra ~
        si.dec ~
        si.epoch ~
        si.properMotion.map(_.ra.μasy.value) ~
        si.properMotion.map(_.dec.μasy.value) ~
        si.radialVelocity ~
        si.parallax ~ // TODO
        si.catalogInfo.flatMap(_.name) ~
        si.catalogInfo.flatMap(_.id) ~
        si.catalogInfo.flatMap(_.objectType) ~
        sourceProfile
      )
    }

    def insertNonsiderealFragment(
      pid:  Program.Id,
      name: NonEmptyString,
      ek:  EphemerisKey,
      sourceProfile: Json
    ): AppliedFragment = {
      sql"""
        insert into t_target (
          c_program_id,
          c_name,
          c_type,
          c_nsid_des,
          c_nsid_key_type,
          c_nsid_key,
          c_source_profile
        )
        select
          $program_id,
          $text_nonempty,
          'nonsidereal',
          ${text_nonempty},
          ${ephemeris_key_type},
          ${text_nonempty},
          $json
      """.apply(
        pid ~
        name ~
        NonEmptyString.from(ek.des).toOption.get ~ // we know this is never emptyek.des ~
        ek.keyType ~
        NonEmptyString.from(EphemerisKey.fromString.reverseGet(ek)).toOption.get ~ // we know this is never empty
        sourceProfile
      )
    }

    def whereFragment(pid: Program.Id, user: User): AppliedFragment = {
      val insert =
        user match {
          case GuestUser(id)                => void"WHERE " |+| existsUserAsPi(pid, id)
          case ServiceUser(_, _)            => void""
          case StandardUser(id, role, _, _) =>
            role match {
              case Admin(_)        => void""
              case Ngo(_, partner) => void"WHERE " |+| existsAllocationForPartner(pid, Tag(partner.tag))
              case Pi(_)           => void"WHERE " |+| existsUserAsPi(pid, id) |+| void" OR " |+| existsUserAsCoi(pid, id)
              case Staff(_)        => void""
            }
        }
      insert |+| void" RETURNING c_target_id"
    }

  }

}