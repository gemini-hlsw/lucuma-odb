// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import grackle.Problem
import grackle.Result
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.ProperMotion
import lucuma.core.model.EphemerisKey
import lucuma.core.model.GuestUser
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ServiceUser
import lucuma.core.model.SourceProfile
import lucuma.core.model.StandardRole.Admin
import lucuma.core.model.StandardRole.Ngo
import lucuma.core.model.StandardRole.Pi
import lucuma.core.model.StandardRole.Staff
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.data.Tag
import lucuma.odb.data.TargetRole
import lucuma.odb.graphql.input.CatalogInfoInput
import lucuma.odb.graphql.input.CloneTargetInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.util.Codecs._
import skunk.AppliedFragment
import skunk.Encoder
import skunk.SqlState
import skunk.Transaction
import skunk.Void
import skunk.circe.codec.all._
import skunk.codec.all._
import skunk.implicits._

import Services.Syntax.*
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.service.TargetService.UpdateTargetsResponse.SourceProfileUpdatesFailed
import lucuma.odb.service.TargetService.UpdateTargetsResponse.TrackingSwitchFailed

trait TargetService[F[_]] {
  def createTarget(pid: Program.Id, input: TargetPropertiesInput.Create)(using Transaction[F]): F[Result[Target.Id]]
  def updateTargets(input: TargetPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[Result[List[Target.Id]]]
  def cloneTarget(input: CloneTargetInput)(using Transaction[F]): F[Result[(Target.Id, Target.Id)]]
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

  def instantiate[F[_]: Concurrent](using Services[F]): TargetService[F] =
    new TargetService[F] {

      private def createTargetImpl(pid: Program.Id, input: TargetPropertiesInput.Create)(using Transaction[F]): F[CreateTargetResponse] = {
        val insert: AppliedFragment =
          input.tracking match {
            case Left(s)  => Statements.insertSiderealFragment(pid, input.name, s, input.sourceProfile.asJson, TargetRole.Science)
            case Right(n) => Statements.insertNonsiderealFragment(pid, input.name, n, input.sourceProfile.asJson, TargetRole.Science)
          }
        val where = Statements.whereFragment(pid, user)
        val appl  = insert |+| void" " |+| where
        session.prepareR(appl.fragment.query(target_id)).use { ps =>
          ps.option(appl.argument).map {
            case Some(tid) => Success(tid)
            case None      => NotAuthorized(user)
          } // todo: catch key violation to indicate ProgramNotFound
        }
      }

      override def createTarget(pid: Program.Id, input: TargetPropertiesInput.Create)(using Transaction[F]): F[Result[Target.Id]] =
        createTargetImpl(pid, input).map:
          case NotAuthorized(user)  => OdbError.NotAuthorized(user.id).asFailure
          case ProgramNotFound(pid) => OdbError.InvalidProgram(pid, Some(s"Program ${pid} was not found")).asFailure
          case Success(id)          => Result(id)

      def updateTargets(input: TargetPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[Result[List[Target.Id]]] =
        updateTargetsImpl(input, which).map:
          case UpdateTargetsResponse.Success(selected)                    => Result.success(selected)
          case UpdateTargetsResponse.SourceProfileUpdatesFailed(problems) => Result.Failure(problems.map(p => OdbError.UpdateFailed(Some(p.message)).asProblem))
          case UpdateTargetsResponse.TrackingSwitchFailed(s)              => OdbError.UpdateFailed(Some(s)).asFailure                  

      def updateTargetsImpl(input: TargetPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[UpdateTargetsResponse] =
        updateTargetsʹ(input, which)

      def updateTargetsʹ(input: TargetPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F]): F[UpdateTargetsResponse] =

        // Updates that don't concern source profile
        val nonSourceProfileUpdates: Stream[F, Target.Id] = {
          val update = Statements.updateTargets(input, which, TargetRole.Science)
          Stream.resource(session.prepareR(update.fragment.query(target_id))).flatMap { ps =>
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
              session.prepareR(sql"select c_source_profile from t_target where c_target_id = $target_id".query(jsonb)),
              session.prepareR(sql"update t_target set c_source_profile = $jsonb where c_target_id = $target_id".command)
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
                case Result.Success(ids)      => UpdateTargetsResponse.Success(ids).pure[F]
                case Result.Failure(ps)       => transaction.rollback.as(UpdateTargetsResponse.SourceProfileUpdatesFailed(ps))
                case Result.Warning(ps, ids)  => transaction.rollback.as(UpdateTargetsResponse.SourceProfileUpdatesFailed(ps))
                case Result.InternalError(th) => Concurrent[F].raiseError(th) // ok? or should we do something else here?
              }

            }

          } recover {
            case SqlState.CheckViolation(ex) if ex.constraintName == Some("ra_dec_epoch_all_defined") =>
              UpdateTargetsResponse.TrackingSwitchFailed("Sidereal targets require RA, Dec, and Epoch to be defined.")
          }

      def cloneTargetImpl(input: CloneTargetInput)(using Transaction[F]): F[CloneTargetResponse] =
        import CloneTargetResponse.*

        val pid: F[Option[Program.Id]] =
          session.prepareR(sql"select c_program_id from t_target where c_target_id = $target_id".query(program_id)).use { ps =>
            ps.option(input.targetId)
          }

        def clone(pid: Program.Id): F[Option[Target.Id]] =
          val stmt = Statements.cloneTarget(pid, input.targetId, user)
          session.prepareR(stmt.fragment.query(target_id)).use(_.option(stmt.argument))

        def update(tid: Target.Id): F[Option[UpdateTargetsResponse]] =
          input.SET.traverse(updateTargetsʹ(_, sql"SELECT $target_id".apply(tid)))

        def replaceIn(tid: Target.Id): F[Unit] =
          input.REPLACE_IN.traverse_ { which =>
            val s1 = Statements.dropItcCache(which)
            val s2 = Statements.replaceTargetIn(which, input.targetId, tid)
            session.prepareR(s1.fragment.command).use(_.execute(s1.argument)) >>
            session.prepareR(s2.fragment.command).use(_.execute(s2.argument))
          }

        pid.flatMap {
          case None => NoSuchTarget(input.targetId).pure[F] // doesn't exist at all
          case Some(pid) =>
            clone(pid).flatMap {
              case None => NoSuchTarget(input.targetId).pure[F] // not authorized
              case Some(tid) =>
                update(tid).flatMap {
                  case Some(err: UpdateTargetsError) => transaction.rollback.as(UpdateFailed(err))
                  case _ => replaceIn(tid) as Success(input.targetId, tid)
                }
            }
        }

      def cloneTarget(input: CloneTargetInput)(using Transaction[F]): F[Result[(Target.Id, Target.Id)]] =
        cloneTargetImpl(input).map:
          case CloneTargetResponse.Success(oldTargetId, newTargetId) => Result((oldTargetId, newTargetId))
          case CloneTargetResponse.NoSuchTarget(targetId) => OdbError.InvalidTarget(targetId, Some(s"No such target: $targetId")).asFailure
          case CloneTargetResponse.UpdateFailed(problem)  =>
            problem match
              case SourceProfileUpdatesFailed(ps) => Result.Failure(ps.map(p => OdbError.UpdateFailed(Some(p.message)).asProblem))
              case TrackingSwitchFailed(p)        => OdbError.UpdateFailed(Some(p)).asFailure

    }

  object Statements {

    import ProgramService.Statements.{ existsUserAsPi, existsUserAsCoi, existsAllocationForPartner }

    def insertSiderealFragment(
      pid:           Program.Id,
      name:          NonEmptyString,
      si:            SiderealInput.Create,
      sourceProfile: Json,
      role:          TargetRole
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
          c_source_profile,
          c_role
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
          $json,
          $target_role
      """.apply(
        pid,
        name,
        si.ra,
        si.dec,
        si.epoch,
        si.properMotion.map(_.ra.μasy.value),
        si.properMotion.map(_.dec.μasy.value),
        si.radialVelocity,
        si.parallax, // TODO
        si.catalogInfo.flatMap(_.name),
        si.catalogInfo.flatMap(_.id),
        si.catalogInfo.flatMap(_.objectType),
        sourceProfile,
        role
      )
    }

    def insertNonsiderealFragment(
      pid:           Program.Id,
      name:          NonEmptyString,
      ek:            EphemerisKey,
      sourceProfile: Json,
      role:          TargetRole
    ): AppliedFragment = {
      sql"""
        insert into t_target (
          c_program_id,
          c_name,
          c_type,
          c_nsid_des,
          c_nsid_key_type,
          c_nsid_key,
          c_source_profile,
          c_role
        )
        select
          $program_id,
          $text_nonempty,
          'nonsidereal',
          ${text_nonempty},
          ${ephemeris_key_type},
          ${text_nonempty},
          $json,
          $target_role
      """.apply(
        pid,
        name,
        NonEmptyString.from(ek.des).toOption.get, // we know this is never emptyek.des ~
        ek.keyType,
        NonEmptyString.from(EphemerisKey.fromString.reverseGet(ek)).toOption.get, // we know this is never empty
        sourceProfile,
        role
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
      role:  TargetRole
    ): AppliedFragment =  {
      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_target "                                         |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "          |+|
          void"WHERE t_target.c_target_id IN (" |+| which |+| void") " |+|
          sql"AND t_target.c_role = $target_role ".apply(role)          |+|
          void"RETURNING t_target.c_target_id"
      updates(SET).fold(which)(update)
    }

    // an exact clone, except for c_target_id and c_existence (which are defaulted)
    def cloneTarget(pid: Program.Id, tid: Target.Id, user: User): AppliedFragment =
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
          c_source_profile,
          c_role
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
          c_source_profile,
          c_role
        FROM t_target
        WHERE c_target_id = $target_id
        AND   c_role      = $target_role
      """.apply(tid, TargetRole.Science) |+|
      ProgramService.Statements.existsUserAccess(user, pid).foldMap(void"AND " |+| _) |+|
      void"""
        RETURNING c_target_id
      """

    def dropItcCache(which: NonEmptyList[Observation.Id]): AppliedFragment =
      sql"""
      DELETE FROM t_itc_result
      WHERE  c_observation_id IN (${observation_id.list(which.length)})
      """.apply(which.toList)

    def replaceTargetIn(which: NonEmptyList[Observation.Id], from: Target.Id, to: Target.Id): AppliedFragment =
      sql"""
      UPDATE t_asterism_target
      SET    c_target_id = $target_id
      WHERE  c_target_id = $target_id
      AND    c_observation_id IN (${observation_id.list(which.length)})
      """.apply(to, from, which.toList)
  }

}
