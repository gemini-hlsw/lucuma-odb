// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import grackle.Problem
import grackle.Result
import grackle.ResultT
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ArcType
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.TargetDisposition
import lucuma.core.math.Angular
import lucuma.core.math.Arc
import lucuma.core.math.Arc.Empty
import lucuma.core.math.Arc.Full
import lucuma.core.math.Arc.Partial
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.model.Ephemeris
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.input.CatalogInfoInput
import lucuma.odb.graphql.input.CloneTargetInput
import lucuma.odb.graphql.input.NonsiderealInput
import lucuma.odb.graphql.input.OpportunityInput
import lucuma.odb.graphql.input.RegionInput
import lucuma.odb.graphql.input.SiderealInput
import lucuma.odb.graphql.input.TargetPropertiesInput
import lucuma.odb.graphql.input.TargetResolutionInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.graphql.mapping.AccessControl.CheckedWithId
import lucuma.odb.json.angle.query.given
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.json.wavelength.query.given
import lucuma.odb.otel.ProgramIdKey
import lucuma.odb.otel.given
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.service.TargetService.UpdateTargetsResponse.SourceProfileUpdatesFailed
import lucuma.odb.service.TargetService.UpdateTargetsResponse.TrackingSwitchFailed
import lucuma.odb.util.Codecs.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.trace.Tracer
import skunk.AppliedFragment
import skunk.Codec
import skunk.Encoder
import skunk.SqlState
import skunk.Transaction
import skunk.Void
import skunk.circe.codec.all.*
import skunk.codec.all.*
import skunk.implicits.*

import Services.Syntax.*

trait TargetService[F[_]] {
  def createTarget(
    input: CheckedWithId[TargetPropertiesInput.Create, Program.Id],
    disposition: TargetDisposition = TargetDisposition.Science,
    role: Option[CalibrationRole] = None
  )(using Transaction[F]): F[Result[Target.Id]]
  def updateTargets(checked: AccessControl.Checked[TargetPropertiesInput.Edit])(using Transaction[F]): F[Result[List[Target.Id]]]
  def cloneTarget(input: AccessControl.CheckedWithId[CloneTargetInput, Program.Id])(using Transaction[F]): F[Result[(Target.Id, Target.Id)]]
  def cloneTargetInto(targetId: Target.Id, programId: Program.Id)(using Transaction[F], ServiceAccess): F[Result[(Target.Id, Target.Id)]]
  def deleteOrphanCalibrationTargets(pid: Program.Id)(using Transaction[F], ServiceAccess): F[Result[Unit]]
}

object TargetService {

  sealed trait CreateTargetResponse
  object CreateTargetResponse {
    case class NotAuthorized(user: User)        extends CreateTargetResponse
    case class ProgramNotFound(pid: Program.Id) extends CreateTargetResponse
    case class Success(id: Target.Id)           extends CreateTargetResponse
  }

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
    case NoSuchProgram(programId: Program.Id)
    case UpdateFailed(problem: UpdateTargetsError)

  def instantiate[F[_]: {Concurrent, Services, Tracer as T, Logger as L}]: TargetService[F] =
    new TargetService[F] {

      override def createTarget(
        input: CheckedWithId[TargetPropertiesInput.Create, Program.Id],
        disposition: TargetDisposition = TargetDisposition.Science,
        role: Option[CalibrationRole] = None
      )(using Transaction[F]): F[Result[Target.Id]] =
        input.foldWithId(OdbError.NotAuthorized(user.id).asFailureF): (input, pid) =>
          val insertFragment: ResultT[F, AppliedFragment] = input.subtypeInfo match
            case s: SiderealInput.Create  => Statements.insertSiderealFragment(pid, input.name, s, input.sourceProfile.asJson, disposition, role).pure
            case OpportunityInput.Create(r, res) =>
              // A resolution given at creation follows the same path the top-level subtypes do,
              // including creating a user-supplied ephemeris first when that is what was given.
              val rResolution: ResultT[F, Option[SiderealInput.Create | Ephemeris.Key]] =
                res match
                  case None                                        => ResultT.pure(None)
                  case Some(s: SiderealInput.Create)               => ResultT.pure(Some(s))
                  case Some(NonsiderealInput.Create.Horizons(k))   => ResultT.pure(Some(k))
                  case Some(NonsiderealInput.Create.UserSupplied(elems)) =>
                    ResultT(Services.asSuperUser(trackingService.createUserSuppliedEphemeris(elems))).map(Some(_))
              rResolution.map: res =>
                Statements.insertOpportunityFragment(pid, input.name, r, res, input.sourceProfile.asJson, disposition, role)
            case NonsiderealInput.Create.Horizons(k) => Statements.insertNonsiderealFragment(pid, input.name, k, input.sourceProfile.asJson, disposition, role).pure
            case NonsiderealInput.Create.UserSupplied(elems) =>
              ResultT(Services.asSuperUser(trackingService.createUserSuppliedEphemeris(elems))).map: k =>
                 Statements.insertNonsiderealFragment(pid, input.name, k, input.sourceProfile.asJson, disposition, role)
          insertFragment
            .flatMap: af =>
              ResultT.liftF:
                session.prepareR(af.fragment.query(target_id)).use: ps =>
                  ps.unique(af.argument)
            .value

      override def updateTargets(checked: AccessControl.Checked[TargetPropertiesInput.Edit])(using Transaction[F]): F[Result[List[Target.Id]]] =
        checked.fold(Result(Nil).pure[F]): (props, which) =>
          Services.asSuperUser(updateTargetsImpl(props, which)).map:
            case UpdateTargetsResponse.Success(selected)                    => Result.success(selected)
            case UpdateTargetsResponse.SourceProfileUpdatesFailed(problems) => Result.Failure(problems.map(p => OdbError.UpdateFailed(Some(p.message)).asProblem))
            case UpdateTargetsResponse.TrackingSwitchFailed(s)              => OdbError.UpdateFailed(Some(s)).asFailure

      private def updateTargetsImpl(input: TargetPropertiesInput.Edit, which: AppliedFragment)(using Transaction[F], SuperUserAccess): F[UpdateTargetsResponse] =

        // Updates to the user-supplied ephemeris, if any
        val replaceEphemeris: F[Unit] =
          input
            .subtypeInfo
            .collect:
              case NonsiderealInput.Edit.UserSupplied(k, elems) =>
                trackingService.replaceUserSuppliedEphemeris(Ephemeris.UserSupplied(k, elems))
            .sequence_

        // Updates that don't concern source profile
        val nonSourceProfileUpdates: Stream[F, Target.Id] = {
          val update = Statements.updateTargets(input, which)
          Stream.resource(session.prepareR(update.fragment.query(target_id))).flatMap { ps =>
            ps.stream(update.argument, 1024)
          }
        }

        replaceEphemeris >> {
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
        }

      private def clone(targetId: Target.Id, pid: Program.Id): F[Target.Id] =
        val stmt = Statements.cloneTarget(pid, targetId)
        session.prepareR(stmt.fragment.query(target_id)).use(_.unique(stmt.argument))

      override def cloneTarget(input: AccessControl.CheckedWithId[CloneTargetInput, Program.Id])(using Transaction[F]): F[Result[(Target.Id, Target.Id)]] =
        input.foldWithId(OdbError.NotAuthorized(user.id).asFailureF): (input, pid) =>
          import CloneTargetResponse.*
          def update(tid: Target.Id): F[Option[UpdateTargetsResponse]] =
            input.SET.traverse(Services.asSuperUser(updateTargetsImpl(_, sql"SELECT $target_id".apply(tid))))

          def replaceIn(tid: Target.Id): F[Unit] =
            input.REPLACE_IN.traverse_ { which =>
              val s1 = Statements.dropItcCache(which)
              val s2 = Statements.replaceTargetIn(which, input.targetId, tid)
              session.prepareR(s1.fragment.command).use(_.execute(s1.argument)) >>
              session.prepareR(s2.fragment.command).use(_.execute(s2.argument))
            }

          clone(input.targetId, pid).flatMap { tid =>
            update(tid).flatMap {
              case Some(err: UpdateTargetsError) => transaction.rollback.as(UpdateFailed(err))
              case _ => replaceIn(tid) as Success(input.targetId, tid)
            }.map(cloneResultTranslation)
          }

      private def cloneTargetIntoImpl(targetId: Target.Id, programId: Program.Id): F[CloneTargetResponse] = {
        import CloneTargetResponse.*

        // Ensure the destination program exists
        val pid: F[Option[Program.Id]] =
          session.prepareR(sql"select c_program_id from t_program where c_program_id = $program_id".query(program_id)).use { ps =>
            ps.option(programId)
          }

        pid.flatMap {
          case None      => NoSuchProgram(programId).pure[F]
          case Some(pid) =>
            clone(targetId, pid).flatMap: tid =>
              Success(targetId, tid).pure[F]
        }
      }

      private def cloneResultTranslation: PartialFunction[CloneTargetResponse, Result[(Target.Id, Target.Id)]] =
        case CloneTargetResponse.Success(oldTargetId, newTargetId) => Result((oldTargetId, newTargetId))
        case CloneTargetResponse.NoSuchProgram(programId)          => OdbError.InvalidProgram(programId, Some(s"No such program: $programId")).asFailure
        case CloneTargetResponse.NoSuchTarget(targetId)            => OdbError.InvalidTarget(targetId, Some(s"No such target: $targetId")).asFailure
        case CloneTargetResponse.UpdateFailed(problem)             =>
          problem match
            case SourceProfileUpdatesFailed(ps) => Result.Failure(ps.map(p => OdbError.UpdateFailed(Some(p.message)).asProblem))
            case TrackingSwitchFailed(p)        => OdbError.UpdateFailed(Some(p)).asFailure

      override def cloneTargetInto(targetId: Target.Id, programId: Program.Id)(using Transaction[F], ServiceAccess): F[Result[(Target.Id, Target.Id)]] =
        cloneTargetIntoImpl(targetId, programId).map(cloneResultTranslation)

      override def deleteOrphanCalibrationTargets(pid: Program.Id)(using Transaction[F], ServiceAccess): F[Result[Unit]] = {
        val s = Statements.deleteOrphanCalibrationTargets(pid)

        T.span("orphan-calibration-targets", Attribute.from(ProgramIdKey, pid)).surround:
          info"Delete orphan calibration targets for $pid" *>
            session
              .prepareR(s.fragment.command)
              .use(_.execute(s.argument))
              .flatTap(r => debug"Orphan target deletion result: $r")
              .as(Result.unit)
      }
    }

  object Statements {

    def insertSiderealFragment(
      pid:           Program.Id,
      name:          NonEmptyString,
      si:            SiderealInput.Create,
      sourceProfile: Json,
      disposition:   TargetDisposition,
      role:          Option[CalibrationRole]
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
          c_target_disposition,
          c_calibration_role
        )
        select
          $program_id,
          $text_nonempty,
          'sidereal',
          ${right_ascension},
          ${declination},
          ${epoch},
          ${int8},
          ${int8},
          ${radial_velocity},
          ${parallax},
          ${catalog_name.opt},
          ${text_nonempty.opt},
          ${text_nonempty.opt},
          $json,
          $target_disposition,
          ${calibration_role.opt}
        returning c_target_id
      """.apply(
        pid,
        name,
        si.ra,
        si.dec,
        si.epoch,
        si.properMotion.fold(0L)(_.ra.μasy.value),
        si.properMotion.fold(0L)(_.dec.μasy.value),
        si.radialVelocity.getOrElse(RadialVelocity.Zero),
        si.parallax.getOrElse(Parallax.Zero),
        si.catalogInfo.flatMap(_.name),
        si.catalogInfo.flatMap(_.id),
        si.catalogInfo.flatMap(_.objectType),
        sourceProfile,
        disposition,
        role
      )
    }

    def insertNonsiderealFragment(
      pid:           Program.Id,
      name:          NonEmptyString,
      ek:            Ephemeris.Key,
      sourceProfile: Json,
      disposition:   TargetDisposition,
      role:          Option[CalibrationRole]
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
          c_target_disposition,
          c_calibration_role
        )
        select
          $program_id,
          $text_nonempty,
          'nonsidereal',
          ${text_nonempty},
          ${ephemeris_key_type},
          ${text_nonempty},
          $json,
          $target_disposition,
          ${calibration_role.opt}
        returning c_target_id
      """.apply(
        pid,
        name,
        NonEmptyString.from(ek.des).toOption.get, // we know this is never emptyek.des ~
        ek.keyType,
        NonEmptyString.from(Ephemeris.Key.fromString.reverseGet(ek)).toOption.get, // we know this is never empty
        sourceProfile,
        disposition,
        role
      )
    }

    def arc[A: Angular](element: Codec[A]): Codec[Arc[A]] =
      (arc_type *: element.opt *: element.opt)
        .eimap[Arc[A]] {
          case (ArcType.Empty, None, None) => Arc.Empty().asRight
          case (ArcType.Full, None, None) => Arc.Full().asRight
          case (ArcType.Partial, Some(s), Some(e)) => Arc.Partial(s, e).asRight
          case (t, s, e) => s"Invalid arc: ($t, $s, $e)}".asLeft
        } {
          case Arc.Empty() => (ArcType.Empty, None, None)
          case Arc.Full() => (ArcType.Full, None, None)
          case Arc.Partial(s, e) => (ArcType.Partial, Some(s), Some(e))
        }

    /**
     * A Target of Opportunity, optionally already resolved. The resolution is stored in the same
     * columns the corresponding target subtype would use, so every one of them appears here and
     * is null when the target is unresolved -- which is the ordinary case at proposal time.
     */
    def insertOpportunityFragment(
      pid:           Program.Id,
      name:          NonEmptyString,
      region:        RegionInput.Create,
      resolution:    Option[SiderealInput.Create | Ephemeris.Key],
      sourceProfile: Json,
      disposition:   TargetDisposition,
      role:          Option[CalibrationRole]
    ): AppliedFragment = {

      val sidereal: Option[SiderealInput.Create] =
        resolution.collect { case s: SiderealInput.Create => s }

      val nonsidereal: Option[Ephemeris.Key] =
        resolution.collect { case k: Ephemeris.Key => k }

      val resolvedType: Option[String] =
        resolution.map:
          case _: SiderealInput.Create => "sidereal"
          case _: Ephemeris.Key        => "nonsidereal"

      sql"""
        insert into t_target (
          c_program_id,
          c_name,
          c_type,
          c_opp_ra_arc_type,
          c_opp_ra_arc_start,
          c_opp_ra_arc_end,
          c_opp_dec_arc_type,
          c_opp_dec_arc_start,
          c_opp_dec_arc_end,
          c_resolved_type,
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
          c_target_disposition,
          c_calibration_role
        )
        select
          $program_id,
          $text_nonempty,
          'opportunity',
          ${arc(right_ascension)},
          ${arc(declination)},
          ${varchar.opt}::e_target_tracking_type,
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
          ${text_nonempty.opt},
          ${ephemeris_key_type.opt},
          ${text_nonempty.opt},
          $json,
          $target_disposition,
          ${calibration_role.opt}
        returning c_target_id
      """.apply(
        pid,
        name,
        region.raArc,
        region.decArc,
        resolvedType,
        sidereal.map(_.ra),
        sidereal.map(_.dec),
        sidereal.map(_.epoch),
        sidereal.map(_.properMotion.fold(0L)(_.ra.μasy.value)),
        sidereal.map(_.properMotion.fold(0L)(_.dec.μasy.value)),
        sidereal.map(_.radialVelocity.getOrElse(RadialVelocity.Zero)),
        sidereal.map(_.parallax.getOrElse(Parallax.Zero)),
        sidereal.flatMap(_.catalogInfo.flatMap(_.name)),
        sidereal.flatMap(_.catalogInfo.flatMap(_.id)),
        sidereal.flatMap(_.catalogInfo.flatMap(_.objectType)),
        nonsidereal.flatMap(k => NonEmptyString.from(k.des).toOption),
        nonsidereal.map(_.keyType),
        nonsidereal.flatMap(k => NonEmptyString.from(Ephemeris.Key.fromString.reverseGet(k)).toOption),
        sourceProfile,
        disposition,
        role
      )
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

    extension [A](a: Arc[A]) def tpe: ArcType =
      a match
        case Empty() => ArcType.Empty
        case Full() => ArcType.Full
        case Partial(_, _) => ArcType.Partial

    // When we update tracking, set the opposite tracking fields to null.
    // If this causes a constraint error it means that the user changed the target type but did not
    // specify every field. We can catch this case and report a useful error.
    def subtypeInfoUpdates(tracking: SiderealInput.Edit | NonsiderealInput.Edit | OpportunityInput.Edit): List[AppliedFragment] =

      // The columns each subtype owns.  Named once, because more than one thing below has to
      // enumerate them and a list that drifts would leave a stray column behind -- which the
      // *_or_all_columns_null constraints reject, at the far end of a mutation.
      val NonsiderealFields =
        List("c_nsid_des", "c_nsid_key_type", "c_nsid_key")

      val SiderealFields =
        List("c_sid_ra", "c_sid_dec", "c_sid_epoch", "c_sid_pm_ra", "c_sid_pm_dec", "c_sid_rv",
             "c_sid_parallax", "c_sid_catalog_name", "c_sid_catalog_id", "c_sid_catalog_object_type")

      val OpportunityFields =
        List("c_opp_ra_arc_type", "c_opp_ra_arc_start", "c_opp_ra_arc_end",
             "c_opp_dec_arc_type", "c_opp_dec_arc_start", "c_opp_dec_arc_end")

      def nullOut(columns: List[String]): List[AppliedFragment] =
        columns.map(col => sql"#$col = null".apply(Void))

      val NullOutNonsiderealFields = nullOut(NonsiderealFields)
      val NullOutSiderealFields    = nullOut(SiderealFields)
      val NullOutOpportunityFields = nullOut(OpportunityFields)

      // The column updates for a tracking edit, without the discriminator.  A resolution is
      // stored in exactly the same columns as the target subtype it resolves to, so this is
      // shared by the subtype change (which sets c_type) and by resolving an opportunity
      // target (which sets c_resolved_type instead).
      def trackingUpdates(tracking: SiderealInput.Edit | NonsiderealInput.Edit): List[AppliedFragment] =
        tracking match

          case sid: SiderealInput.Edit =>
            List(
              sid.ra.asUpdate("c_sid_ra", right_ascension),
              sid.dec.asUpdate("c_sid_dec", declination),
              sid.epoch.asUpdate("c_sid_epoch", epoch),
              sid.radialVelocity.asUpdate("c_sid_rv", radial_velocity),
              sid.parallax.asUpdate("c_sid_parallax", parallax),
            ).flatten ++
            properMotionUpdates(sid.properMotion) ++
            catalogInfoUpdates(sid.catalogInfo) ++
            NullOutNonsiderealFields

          case e: NonsiderealInput.Edit =>
            List(
              sql"c_nsid_des = $text".apply(e.key.des),
              sql"c_nsid_key_type = $ephemeris_key_type".apply(e.key.keyType),
              sql"c_nsid_key = $text".apply(Ephemeris.Key.fromString.reverseGet(e.key)),
            ) ++
            NullOutSiderealFields

      // Clear the tracking columns of a target being *converted* into an opportunity target.
      // This arm always sets c_type = 'opportunity', so it serves two different edits: touching
      // an existing opportunity target's region, which must not silently un-resolve it, and
      // converting a sidereal or nonsidereal target into one, which must not leave the old
      // coordinates behind -- with no resolution they belong to no tracking type, and
      // sidereal_or_all_columns_null rejects the row.
      //
      // Only the ELSE branch does anything; for a target that is already an opportunity target
      // each assignment is a no-op.  It is written this way because a single UPDATE covers a set
      // of ids that may hold both kinds, so the two cases can only be told apart per row.  The
      // right-hand side of an UPDATE sees the OLD row, so c_type here is the type the target had
      // before this statement set it to 'opportunity'.
      val ClearTrackingUnlessAlreadyOpportunity =
        (List("c_resolved_type") ++ SiderealFields ++ NonsiderealFields).map: col =>
          sql"#$col = CASE WHEN c_type = 'opportunity' THEN #$col ELSE null END".apply(Void)

      // Resolving an opportunity target, un-resolving it, or leaving it alone.  Un-resolving
      // must clear the tracking columns as well as the discriminator, for the same constraint
      // reason.  Each arm owns every tracking column it touches, so no column is assigned twice.
      def resolutionUpdates(resolution: Nullable[TargetResolutionInput.Edit]): List[AppliedFragment] =
        resolution match
          case Nullable.Absent      =>
            ClearTrackingUnlessAlreadyOpportunity
          case Nullable.Null        =>
            void"c_resolved_type = null" :: NullOutSiderealFields ++ NullOutNonsiderealFields
          case Nullable.NonNull(t)  =>
            val tag = t match
              case _: SiderealInput.Edit    => void"c_resolved_type = 'sidereal'"
              case _: NonsiderealInput.Edit => void"c_resolved_type = 'nonsidereal'"
            tag :: trackingUpdates(t)

      tracking match {

        case sid: SiderealInput.Edit   =>
          void"c_type = 'sidereal'" ::
          void"c_resolved_type = null" ::
          trackingUpdates(sid) ++
          NullOutOpportunityFields

        case e: NonsiderealInput.Edit =>
          void"c_type = 'nonsidereal'" ::
          void"c_resolved_type = null" ::
          trackingUpdates(e) ++
          NullOutOpportunityFields

        // Note this does NOT null out the tracking columns.  A resolved Target of Opportunity
        // keeps its coordinates, and editing its region must not silently un-resolve it; only
        // an explicit `resolution: null` does that, via resolutionUpdates.
        case opp: OpportunityInput.Edit =>
          // Omitting the region leaves it exactly as it was, which is what lets a client
          // resolve a target without having to restate -- and risk redrawing -- the patch of
          // sky the TAC approved.
          val regionUpdates: List[AppliedFragment] =
            opp.region.toList.flatMap: region =>
              List(
                region.raArc.map(_.tpe).asUpdate("c_opp_ra_arc_type", arc_type),
                region.raArc.map(Arc.start.getOption).asUpdate("c_opp_ra_arc_start", right_ascension.opt),
                region.raArc.map(Arc.end.getOption).asUpdate("c_opp_ra_arc_end", right_ascension.opt),
                region.decArc.map(_.tpe).asUpdate("c_opp_dec_arc_type", arc_type),
                region.decArc.map(Arc.start.getOption).asUpdate("c_opp_dec_arc_start", declination.opt),
                region.decArc.map(Arc.end.getOption).asUpdate("c_opp_dec_arc_end", declination.opt),
              )
          void"c_type = 'opportunity'" :: regionUpdates ++ resolutionUpdates(opp.resolution)

      }

    def updates(SET: TargetPropertiesInput.Edit): Option[NonEmptyList[AppliedFragment]] =
      NonEmptyList.fromList(
        List(
          SET.existence.map(sql"c_existence = $existence"),
          SET.name.map(sql"c_name = $text_nonempty"),
        ).flatten ++
        SET.subtypeInfo.toList.flatMap(subtypeInfoUpdates)
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
      val ifNone =
        void"SELECT c_target_id FROM t_target WHERE c_target_id IN (" |+| which |+| void")"
      updates(SET).fold(ifNone)(update)
    }

    // an exact clone, except for c_target_id and c_existence (which are defaulted)
    def cloneTarget(pid: Program.Id, tid: Target.Id): AppliedFragment =
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
          c_calibration_role,
          c_target_disposition,
          c_resolved_type,
          c_opp_ra_arc_type,
          c_opp_ra_arc_start,
          c_opp_ra_arc_end,
          c_opp_dec_arc_type,
          c_opp_dec_arc_start,
          c_opp_dec_arc_end
        )
        SELECT
          $program_id,
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
          c_calibration_role,
          c_target_disposition,
          c_resolved_type,
          c_opp_ra_arc_type,
          c_opp_ra_arc_start,
          c_opp_ra_arc_end,
          c_opp_dec_arc_type,
          c_opp_dec_arc_start,
          c_opp_dec_arc_end
        FROM t_target
        WHERE c_target_id = $target_id
        RETURNING c_target_id
      """.apply(pid, tid)

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

    def deleteOrphanCalibrationTargets(pid: Program.Id): AppliedFragment =
      sql"""
      DELETE FROM t_target
      WHERE  c_calibration_role IS NOT NULL AND
             c_target_disposition = 'calibration' AND
             c_program_id = $program_id AND NOT EXISTS (
               SELECT 1 FROM t_asterism_target WHERE c_target_id = t_target.c_target_id
             )
      """.apply(pid)
  }

}
