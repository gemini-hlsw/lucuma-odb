// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.effect.*
import cats.syntax.all._
import edu.gemini.grackle.Problem
import edu.gemini.grackle.Result
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.ProperMotion
import lucuma.core.model.CatalogInfo
import lucuma.core.model.EphemerisKey
import lucuma.core.model.GuestUser
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ServiceUser
import lucuma.core.model.SiderealTracking
import lucuma.core.model.SourceProfile
import lucuma.core.model.StandardRole.Admin
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Staff
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.*
import lucuma.odb.data.Tag
import lucuma.odb.graphql.input.CatalogInfoInput
import lucuma.odb.graphql.input.CloneTargetInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.input.UpdateTargetsInput
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.util.Codecs._
import skunk.AppliedFragment
import skunk.Encoder
import skunk.Query
import skunk.Session
import skunk.SqlState
import skunk.Transaction
import skunk.Void
import skunk.circe.codec.all._
import skunk.codec.all._
import skunk.implicits._

trait TargetService[F[_]] {
  import TargetService.{ CloneTargetResponse, CreateTargetResponse, UpdateTargetsResponse }
  def createTarget(pid: Program.Id, input: TargetPropertiesInput.Create): F[CreateTargetResponse]
  def updateTargets(input: TargetPropertiesInput.Edit, which: AppliedFragment): F[UpdateTargetsResponse]
  def cloneTarget(input: CloneTargetInput): F[CloneTargetResponse]
}

object TargetService {

  sealed trait CreateTargetResponse
  object CreateTargetResponse {
    case class NotAuthorized(user: User)        extends CreateTargetResponse
    case class ProgramNotFound(pid: Program.Id) extends CreateTargetResponse
    case class Success(id: Target.Id)           extends CreateTargetResponse
  }
  import CreateTargetResponse._

  sealed trait UpdateTargetsResponse
  sealed trait UpdateTargetsError extends UpdateTargetsResponse
  object UpdateTargetsResponse {
    case class Success(selected: List[Target.Id]) extends UpdateTargetsResponse
    case class SourceProfileUpdatesFailed(problems: NonEmptyChain[Problem]) extends UpdateTargetsError
    case class TrackingSwitchFailed(problem: String) extends UpdateTargetsError
  }

  enum CloneTargetResponse:
    case Success(oldTargetId: Target.Id, newTargetId: Target.Id)
    case NoSuchTarget(targetId: Target.Id)
    case UpdateFailed(problem: UpdateTargetsError)

  def fromSession[F[_]: Concurrent](s: Session[F], u: User): TargetService[F] =
    new TargetService[F] {

      override def createTarget(pid: Program.Id, input: TargetPropertiesInput.Create): F[CreateTargetResponse] = {
        val insert: AppliedFragment =
          input.tracking match {
            case Left(s)  => Statements.insertSiderealFragment(pid, input.name, s, input.sourceProfile.asJson)
            case Right(n) => Statements.insertNonsiderealFragment(pid, input.name, n, input.sourceProfile.asJson)
          }
        val where = Statements.whereFragment(pid, u)
        val appl  = insert |+| void" " |+| where
        s.prepareR(appl.fragment.query(target_id)).use { ps =>
          ps.option(appl.argument).map {
            case Some(tid) => Success(tid)
            case None      => NotAuthorized(u)
          } // todo: catch key violation to indicate ProgramNotFound
        }
      }

      def updateTargets(input: TargetPropertiesInput.Edit, which: AppliedFragment): F[UpdateTargetsResponse] =
        s.transaction.use { xa =>
          updateTargetsʹ(xa, input, which)
        }

      def updateTargetsʹ(xa: Transaction[F], input: TargetPropertiesInput.Edit, which: AppliedFragment): F[UpdateTargetsResponse] =

        // Updates that don't concern source profile
        val nonSourceProfileUpdates: Stream[F, Target.Id] = {
          val update = Statements.updateTargets(input, which)
          Stream.resource(s.prepareR(update.fragment.query(target_id))).flatMap { ps =>
            ps.stream(update.argument, 1024)
          }          
        }

        input.sourceProfile match {
          case None =>

            // We're not updating the source profile, so we're basically done
            nonSourceProfileUpdates.compile.toList.map(UpdateTargetsResponse.Success(_))

          case Some(fun) =>

            // Here we must (embarrassingly) update each source profile individually, but we can
            // save a little bit of overhaed by preparing the statements once and reusing them.
            Stream.resource((
              s.prepareR(sql"select c_source_profile from t_target where c_target_id = $target_id".query(jsonb)),
              s.prepareR(sql"update t_target set c_source_profile = $jsonb where c_target_id = $target_id".command)
            ).tupled).flatMap { (read, update) =>
              nonSourceProfileUpdates.evalMap { tid =>
                read.unique(tid).map(_.hcursor.as[SourceProfile]).flatMap {
                  case Left(err) => Result.failure(err.getMessage).pure[F]
                  case Right(sp) => fun(sp).map(_.asJson).traverse(update.execute(_, tid).as(tid))
                }
              }
            } .compile.toList.map(_.sequence).flatMap { r =>

              // If any source profile updates failed then roll back.
              r match {
                case Ior.Left(ps)    => xa.rollback.as(UpdateTargetsResponse.SourceProfileUpdatesFailed(ps))
                case Ior.Both(ps, _) => xa.rollback.as(UpdateTargetsResponse.SourceProfileUpdatesFailed(ps))
                case Ior.Right(ids)  => UpdateTargetsResponse.Success(ids).pure[F]
              }
              
            }

          } recover {
            case SqlState.CheckViolation(ex) if ex.constraintName == Some("ra_dec_epoch_all_defined") =>
              UpdateTargetsResponse.TrackingSwitchFailed("Sidereal targets require RA, Dec, and Epoch to be defined.")
          }

      def cloneTarget(input: CloneTargetInput): F[CloneTargetResponse] =
        s.transaction.use { xa =>
          
          // TODO: need to ensure that the original target is visible to the user          

          val clone: F[Target.Id] =
            s.prepareR(Statements.cloneTarget).use(_.unique(input.targetId))

          def update(tid: Target.Id): F[Option[UpdateTargetsResponse]] = 
            input.SET.traverse(updateTargetsʹ(xa, _, sql"$target_id".apply(tid)))
          
          def replaceIn(tid: Target.Id): F[Unit] =
            input.REPLACE_IN.traverse_ { which =>
              val stmt = Statements.replaceTargetIn(which, input.targetId, tid)
              s.prepareR(stmt.fragment.command).use(_.execute(stmt.argument))               
            } 

          clone.flatMap { tid =>
            update(tid).flatMap {              
                case Some(err: UpdateTargetsError) => CloneTargetResponse.UpdateFailed(err).pure[F]         
                case _ => replaceIn(tid) as CloneTargetResponse.Success(input.targetId, tid)
            }
          }

        }

    }

  object Statements {

    import ProgramService.Statements.{ existsUserAsPi, existsUserAsCoi, existsAllocationForPartner }

    def insertSiderealFragment(
      pid:  Program.Id,
      name: NonEmptyString,
      si:   SiderealInput.Create,
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
          ${right_ascension},
          ${declination},
          ${epoch},
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

    extension [A](n: Nullable[A]) def asUpdate(column: String, e: Encoder[A]): Option[AppliedFragment] =
      n.foldPresent(_.fold(void"null")(sql"$e")).map(sql"#$column = "(Void) |+| _)

    extension [A](n: Option[A]) def asUpdate(column: String, e: Encoder[A]): AppliedFragment =
      n.fold(sql"#$column = null"(Void))(sql"#$column = $e")

    // Either no updates, set the Proper Motion to null, or set it to the provided value
    def properMotionUpdates(npm: Nullable[ProperMotion]): List[AppliedFragment] =
      npm.toOptionOption.toList.map {
        case None     => (None, None)
        case Some(pm) => (Some(pm.ra.μasy.value), Some(pm.dec.μasy.value))
      } flatMap { (ora, odec) =>
        List(
            sql"c_sid_pm_ra = ${int8.opt}"(ora),
            sql"c_sid_pm_dec = ${int8.opt}"(odec),
          )
      }
      
    // Either no updates, set the Catalog Info to null, or set it to the provided value
    def catalogInfoUpdates(nci: Nullable[CatalogInfoInput]): List[AppliedFragment] =
      nci.toOptionOption.toList.map(_.getOrElse(CatalogInfoInput.Empty)).flatMap { ci =>
        List(
          sql"c_sid_catalog_name = ${catalog_name.opt}"(ci.name),
          sql"c_sid_catalog_id = ${text_nonempty.opt}"(ci.id),
          sql"c_sid_catalog_object_type = ${text_nonempty.opt}"(ci.objectType),
        )
      }

    // When we update tracking, set the opposite tracking fields to null.
    // If this causes a constraint error it means that the user changed the target type but did not
    // specify every field. We can catch this case and report a useful error.
    def trackingUpdates(tracking: Either[SiderealInput.Edit, EphemerisKey]): List[AppliedFragment] =
      tracking match {
        case Left(sid)   => 
          List(
            sid.ra.asUpdate("c_sid_ra", right_ascension),
            sid.dec.asUpdate("c_sid_dec", declination),
            sid.epoch.asUpdate("c_sid_epoch", epoch),
            sid.radialVelocity.asUpdate("c_sid_rv", radial_velocity),          
            sid.parallax.asUpdate("c_sid_parallax", parallax),
          ).flatten ++
          properMotionUpdates(sid.properMotion) ++
          catalogInfoUpdates(sid.catalogInfo) ++
          List(
            // set the type, and set inapplicable tracking fields to null
            void"c_type = 'sidereal'",
            void"c_nsid_des = null",
            void"c_nsid_key_type = null",
            void"c_nsid_key = null",
          )
        case Right(ek) =>
          List(
            sql"c_nsid_des = $text".apply(ek.des),
            sql"c_nsid_key_type = $ephemeris_key_type".apply(ek.keyType),
            sql"c_nsid_key = $text".apply(EphemerisKey.fromString.reverseGet(ek)),
          ) ++ List(
            // set the type, and set inapplicable tracking fields to null
            void"c_type = 'nonsidereal'",
            void"c_sid_ra = null",
            void"c_sid_dec = null",
            void"c_sid_epoch = null",
            void"c_sid_pm_ra = null",
            void"c_sid_pm_dec = null",
            void"c_sid_rv = null",
            void"c_sid_parallax = null",
            void"c_sid_catalog_name = null",
            void"c_sid_catalog_id = null",
            void"c_sid_catalog_object_type = null",
          )
      }
      
    def updates(SET: TargetPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      NonEmptyList.fromList(
        List(
          SET.existence.map(sql"c_existence = $existence"),
          SET.name.map(sql"c_name = $text_nonempty"),
        ).flatten ++
        SET.tracking.toList.flatMap(trackingUpdates)
      )    

    // Contruct an update (or am select in the case of no updates) performing the requested updates on
    // targes matching `which` and returning the ids of the affected targets. Note that the source
    // profile edit requires special handling and is ignored here.
    def updateTargets(
      SET:   TargetPropertiesInput.Edit,
      which: AppliedFragment,
    ): AppliedFragment =  {
      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_target "                                         |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "          |+|
          void"WHERE t_target.c_target_id IN (" |+| which |+| void") " |+|
          void"RETURNING t_target.c_target_id"
      updates(SET).fold(which)(update)
    }

    // an exact clone, except for c_target_id and c_existence (which are defaulted)
    val cloneTarget: Query[Target.Id, Target.Id] =
      sql"""
        INSERT INTO t_target(
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
          c_nsid_des,
          c_nsid_key_type,
          c_nsid_key,
          c_source_profile
        )
        SELECT 
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
          c_nsid_des,
          c_nsid_key_type,
          c_nsid_key,
          c_source_profile
        FROM t_target
        WHERE c_target_id = $target_id
        RETURNING c_target_id
      """.query(target_id)

    def replaceTargetIn(which: NonEmptyList[Observation.Id], from: Target.Id, to: Target.Id): AppliedFragment =
      sql"""
      UPDATE t_target_asterism
      SET    c_target_id = $target_id
      WHERE  c_target_id = $target_id
      AND    c_observation_id IN (${observation_id.list(which.length)})
      """.apply(to ~ from ~ which.toList)
  }

}