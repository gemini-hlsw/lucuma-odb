// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.GuestRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole._
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.Tag
import lucuma.odb.data.UpdateResult
import lucuma.odb.graphql.snippet.input.ConstraintSetInput
import lucuma.odb.graphql.snippet.input.ObservationPropertiesInput
import lucuma.odb.util.Codecs._
import natchez.Trace
import skunk._
import skunk.implicits._

trait ObservationService[F[_]] {
  import ObservationService._

  def createObservation(
    programId:   Program.Id,
    SET:         ObservationPropertiesInput,
  ): F[CreateResult]

  // TODO: probably delete
//  def updateObservation(
//    observationId: Observation.Id,
//    SET:           ObservationPropertiesInput
//  ): F[UpdateResult[Observation.Id]]

  def updateObservations(
    WHERE: List[Observation.Id],
    SET:   ObservationPropertiesInput
  ): F[UpdateResult[List[Observation.Id]]]

}


object ObservationService {

  sealed trait CreateResult
  object CreateResult {
    final case class NotAuthorized(user: User)   extends CreateResult
    final case class Success(id: Observation.Id) extends CreateResult
  }

  def fromSessionAndUser[F[_]: Sync: Trace](
    session: Session[F],
    user:    User
  ): ObservationService[F] =
    new ObservationService[F] {

//      def fail[A](msg: String): F[A] =
//        MonadCancelThrow[F].raiseError(new RuntimeException(msg))

      override def createObservation(
        programId:   Program.Id,
        SET:         ObservationPropertiesInput,
      ): F[CreateResult] =
        Trace[F].span("createObservation") {
          val af = Statements.insertObservationAs(
            user,
            programId,
            SET.subtitle.toOption,
            SET.existence.getOrElse(Existence.Default),
            SET.status.getOrElse(ObsStatus.Default),
            SET.activeStatus.getOrElse(ObsActiveStatus.Default),
            SET.constraintSet.getOrElse(ConstraintSetInput.NominalConstraints),
          )
          session.prepare(af.fragment.query(observation_id)).use { pq =>
            pq.option(af.argument).map {
              case Some(oid) => CreateResult.Success(oid)
              case None      => CreateResult.NotAuthorized(user)
            }
          }
        }

      // TODO: probably delete
//      override def updateObservation(
//        observationId: Observation.Id,
//        SET:           ObservationPropertiesInput
//      ): F[UpdateResult[Observation.Id]] =
//        Trace[F].span("updateObservation") {
//          Statements.updateObservation(observationId, SET.subtitle, SET.existence, SET.status, SET.activeStatus, SET.constraintSet, user) match {
//            case None     => UpdateResult.NothingToBeDone.pure[F].widen
//            case Some(af) =>
//              session.prepare(af.fragment.command).use(_.execute(af.argument)).flatMap {
//                case Completion.Update(0) => UpdateResult.NoSuchObject.pure[F].widen
//                case Completion.Update(1) => UpdateResult.Success(observationId).pure[F].widen
//                case other                => fail(s"Expected `Update(0)` or `Update(1)`, found $other")
//              }
//          }
//        }

      override def updateObservations(
        WHERE: List[Observation.Id],
        SET:   ObservationPropertiesInput
      ): F[UpdateResult[List[Observation.Id]]] =
        Trace[F].span("updateObservations") {
          Statements.updateObservations(WHERE, SET.subtitle, SET.existence, SET.status, SET.activeStatus, SET.constraintSet, user) match {
            case None     =>
              UpdateResult.NothingToBeDone.pure[F].widen
            case Some(af) =>
              session.prepare(af.fragment.query(observation_id)).use { pq =>
                pq.stream(af.argument, chunkSize = 1024).compile.toList.map {
                  case Nil  => UpdateResult.NoSuchObject
                  case oids => UpdateResult.Success(oids)
                }
              }
          }
        }

    }


  object Statements {
    import ProgramService.Statements.{ existsUserAsPi, existsUserAsCoi, existsAllocationForPartner }

    def insertObservationAs(
      user:          User,
      programId:     Program.Id,
      subtitle:      Option[NonEmptyString],
      existence:     Existence,
      status:        ObsStatus,
      activeState:   ObsActiveStatus,
      constraintSet: ConstraintSet
    ): AppliedFragment = {

      val insert: AppliedFragment =
        InsertObservation.apply(
          programId    ~
           subtitle    ~
           existence   ~
           status      ~
           activeState ~
           constraintSet.cloudExtinction ~
           constraintSet.imageQuality    ~
           constraintSet.skyBackground   ~
           constraintSet.waterVapor      ~
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.min.value)) ~  // TODO: fix in core
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.max.value)) ~
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.minHours.value)                           ~
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.maxHours.value)
        )

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

    val InsertObservation: Fragment[
      Program.Id             ~
      Option[NonEmptyString] ~
      Existence              ~
      ObsStatus              ~
      ObsActiveStatus        ~
      CloudExtinction        ~
      ImageQuality           ~
      SkyBackground          ~
      WaterVapor             ~
      Option[PosBigDecimal]  ~
      Option[PosBigDecimal]  ~
      Option[BigDecimal]     ~
      Option[BigDecimal]
    ] =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_subtitle,
          c_existence,
          c_status,
          c_active_status,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max
        )
        SELECT
          $program_id,
          ${text_nonempty.opt},
          $existence,
          $obs_status,
          $obs_active_status,
          $cloud_extinction,
          $image_quality,
          $sky_background,
          $water_vapor,
          ${air_mass_range_value.opt},
          ${air_mass_range_value.opt},
          ${hour_angle_range_value.opt},
          ${hour_angle_range_value.opt}
      """

    // TODO: Probably delete
//    def updateObservation(
//      observationId: Observation.Id,
//      subtitle:      Nullable[NonEmptyString],
//      ex:            Option[Existence],
//      status:        Option[ObsStatus],
//      activeState:   Option[ObsActiveStatus],
//      constraintSet: Option[ConstraintSet],
//      user:          User
//    ): Option[AppliedFragment] = {
//
//      val base = void"update t_observation o set "
//
//      val upExistence = sql"c_existence = $existence"
//      val upSubtitle  = sql"c_subtitle = ${text_nonempty.opt}"
//      val upStatus    = sql"c_status = $obs_status"
//      val upActive    = sql"c_active_status = $obs_active_status"
//
//      val upCloud     = sql"c_cloud_extinction = $cloud_extinction"
//      val upImage     = sql"c_image_quality = $image_quality"
//      val upSky       = sql"c_sky_background = $sky_background"
//      val upWater     = sql"c_water_vapor = $water_vapor"
//
//      val ups: List[AppliedFragment] =
//        List(
//          ex.map(upExistence),
//          subtitle match {
//            case Nullable.Null  => Some(upSubtitle(None))
//            case Absent         => None
//            case NonNull(value) => Some(upSubtitle(Some(value)))
//          },
//          status.map(upStatus),
//          activeState.map(upActive),
//          constraintSet.toList.flatMap { cs =>
//            List(
//              upCloud(cs.cloudExtinction),
//              upImage(cs.imageQuality),
//              upSky(cs.skyBackground),
//              upWater(cs.waterVapor)
//            )
//          }
//        ).flatten
//
//      NonEmptyList.fromList(ups).map { nel =>
//        val up = nel.intercalate(void", ")
//
//        import lucuma.core.model.Access._
//
//        val where = user.role.access match {
//
//          case Service | Admin | Staff =>
//            sql"""
//              where o.c_observation_id = $observation_id
//            """.apply(observationId)
//
//          case Ngo => ??? // TODO
//
//          case Guest | Pi =>
//            sql"""
//              from t_program p
//              where o.c_observation_id = $observation_id
//              and o.c_program_id = p.c_program_id
//              and (
//                p.c_pi_user_id = $user_id
//                or
//                exists(
//                  select u.c_role
//                  from   t_program_user u
//                  where  u.c_program_id = o.c_program_id
//                  and    u.c_user_id    = $user_id
//                  and    u.c_role       = 'coi'
//                )
//              )
//            """.apply(observationId ~ user.id ~ user.id)
//        }
//
//        base |+| up |+| where
//
//      }
//    }

    def updateObservations(
      WHERE:         List[Observation.Id],
      subtitle:      Nullable[NonEmptyString],
      ex:            Option[Existence],
      status:        Option[ObsStatus],
      activeState:   Option[ObsActiveStatus],
      constraintSet: Option[ConstraintSet],
      user:          User
    ): Option[AppliedFragment] = {

      val base = void"update t_observation o set "

      val upExistence = sql"c_existence = $existence"
      val upSubtitle  = sql"c_subtitle = ${text_nonempty.opt}"
      val upStatus    = sql"c_status = $obs_status"
      val upActive    = sql"c_active_status = $obs_active_status"

      val upCloud     = sql"c_cloud_extinction = $cloud_extinction"
      val upImage     = sql"c_image_quality = $image_quality"
      val upSky       = sql"c_sky_background = $sky_background"
      val upWater     = sql"c_water_vapor = $water_vapor"

      val ups: List[AppliedFragment] =
        List(
          ex.map(upExistence),
          subtitle match {
            case Nullable.Null  => Some(upSubtitle(None))
            case Absent         => None
            case NonNull(value) => Some(upSubtitle(Some(value)))
          },
          status.map(upStatus),
          activeState.map(upActive),
          constraintSet.toList.flatMap { cs =>
            List(
              upCloud(cs.cloudExtinction),
              upImage(cs.imageQuality),
              upSky(cs.skyBackground),
              upWater(cs.waterVapor)
            )
          }
        ).flatten

      for {
        _   <- WHERE.headOption
        nel <- NonEmptyList.fromList(ups)
      } yield {
        val up = nel.intercalate(void", ")

        import lucuma.core.model.Access._

        val where = user.role.access match {

          case Service | Admin | Staff =>
            sql"""
              where o.c_observation_id in ( ${observation_id.list(WHERE)} )
            """.apply(WHERE)

          case Ngo => ??? // TODO

          case Guest | Pi =>
            sql"""
              from t_program p
              where o.c_observation_id in ( ${observation_id.list(WHERE.size)} )
              and o.c_program_id = p.c_program_id
              and (
                p.c_pi_user_id = $user_id
                or
                exists(
                  select u.c_role
                  from   t_program_user u
                  where  u.c_program_id = o.c_program_id
                  and    u.c_user_id    = $user_id
                  and    u.c_role       = 'coi'
                )
              )
            """.apply(WHERE ~ user.id ~ user.id)
        }

        val returning: AppliedFragment =
          void"RETURNING o.c_observation_id"

        base |+| up |+| where |+| returning

      }
    }

  }


}