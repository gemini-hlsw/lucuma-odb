// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.model.{ConstraintSet, ElevationRange, GuestRole, Observation, Program, ServiceRole, User, nonNegDurationValidate}
import lucuma.core.model.StandardRole._
import lucuma.odb.data.Nullable.{Absent, NonNull}
import lucuma.odb.data.{Existence, Nullable, ObsActiveStatus, ObsStatus, Tag}
import lucuma.odb.graphql.snippet.input.ConstraintSetInput
import lucuma.odb.graphql.snippet.input.ObservationPropertiesInput
import lucuma.odb.util.Codecs._
import skunk._
import skunk.implicits._

trait ObservationService[F[_]] {
  import ObservationService._

  def createObservation(
    programId:   Program.Id,
    SET:         ObservationPropertiesInput,
  ): F[CreateObservationResponse]

  def updateObservation(
    observationId: Observation.Id,
    SET:           ObservationPropertiesInput
  ): F[UpdateObservationResponse]

}


object ObservationService {

  sealed trait CreateObservationResponse
  object CreateObservationResponse {
    final case class NotAuthorized(user: User)   extends CreateObservationResponse
    final case class Success(id: Observation.Id) extends CreateObservationResponse
  }

  sealed trait UpdateObservationResponse
  object UpdateObservationResponse {
    final case class NotAuthorized(user: User)   extends UpdateObservationResponse
    final case class Success(id: Observation.Id) extends UpdateObservationResponse
  }

  def fromUserAndSession[F[_]: MonadCancelThrow](user: User, session: Session[F]): ObservationService[F] =
    new ObservationService[F] {

      def createObservation(
        programId:   Program.Id,
        SET:         ObservationPropertiesInput,
      ): F[CreateObservationResponse] = {
        import CreateObservationResponse._

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
            case Some(oid) => Success(oid)
            case None      => NotAuthorized(user)
          }
        }
      }

      def updateObservation(
        observationId: Observation.Id,
        SET:           ObservationPropertiesInput
      ): F[UpdateObservationResponse] = {

        import UpdateObservationResponse._

        ???
      }

    }


  object Statements {
    import ProgramService.Statements.{ existsUserAsPi, existsUserAsCoi, existsAllocationForPartner }

    def insertObservationAs(
      user:          User,
      programId:     Program.Id,
      name:          Option[NonEmptyString],
      existence:     Existence,
      status:        ObsStatus,
      activeState:   ObsActiveStatus,
      constraintSet: ConstraintSet
    ): AppliedFragment = {

      val insert: AppliedFragment =
        InsertObservation.apply(
          programId    ~
           name        ~
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
          c_name,
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

  }

  def updateObservation(
    observationId: Observation.Id,
    programId:     Program.Id,
    name:          Nullable[NonEmptyString],
    ex:            Option[Existence],
    status:        Option[ObsStatus],
    activeState:   Option[ObsActiveStatus],
    constraintSet: Option[ConstraintSet],
    user:          User
  ): Option[AppliedFragment] = {

    val base = void"update t_observation set "

    val upExistence = sql"c_existence = $existence"
    val upName      = sql"c_name = ${text_nonempty.opt}"

    val ups: List[AppliedFragment] =
      List(
        ex.map(upExistence),
        name match {
          case Nullable.Null  => Some(upName(None))
          case Absent         => None
          case NonNull(value) => Some(upName(Some(value)))
        }
      ).flatten

    NonEmptyList.fromList(ups).map { nel =>
      val up = nel.intercalate(void", ")

      import lucuma.core.model.Access._

      val where = user.role.access match {

        case Service | Admin | Staff =>
          sql"""
            where c_observation_id = $observation_id
          """.apply(observationId)

        case Ngo => ??? // TODO

        case Guest | Pi =>
          sql"""
            where c_observation_id = $observation_id
          """
      }
      ???
    }
    ???
  }

}