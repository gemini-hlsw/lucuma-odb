package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
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
import lucuma.odb.graphql.snippet.input.CreateTargetInput
import lucuma.odb.graphql.snippet.input.SiderealInput
import lucuma.odb.util.Codecs._
import skunk.AppliedFragment
import skunk.Session
import skunk.circe.codec.all._
import skunk.implicits._
import skunk.codec.all._

trait TargetService[F[_]] {
  import TargetService.CreateTargetResponse
  def createTarget(pid: Program.Id, input: CreateTargetInput): F[CreateTargetResponse]
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

        override def createTarget(pid: Program.Id, input: CreateTargetInput): F[CreateTargetResponse] = {
          val insert: AppliedFragment =
            input.tracking match {
              case Left(s) => insertSiderealFragment(pid, input.name, s, input.sourceProfile)
              case Right(n) => ???
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
          0, -- TODO
          0, -- TODO
          ${radial_velocity.opt},
          ${numeric.opt}, -- TODO
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
        si.radialVelocity ~
        si.parallax.as(BigDecimal(0.0)) ~ // TODO
        si.catalogInfo.flatMap(_.name) ~
        si.catalogInfo.flatMap(_.id) ~
        si.catalogInfo.flatMap(_.objectType) ~
        sourceProfile
      )
    }

    def whereFragment(pid: Program.Id, user: User): AppliedFragment = {
      val insert =
        user match {
          case GuestUser(id)                => void"WHERE " |+| existsUserAsPi(pid, id)
          case ServiceUser(id, name)        => void""
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