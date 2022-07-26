// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.{CloudExtinction, ImageQuality, SkyBackground, WaterVapor}
import lucuma.core.model.ElevationRange.{AirMass, HourAngle}
import lucuma.core.model.{ConstraintSet, ElevationRange, GuestRole, Observation, Program, ServiceRole, User}
import lucuma.core.model.StandardRole._
import lucuma.odb.data.Existence
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.Tag
import lucuma.odb.graphql.snippet.input.ConstraintSetInput
import lucuma.odb.util.Codecs._
import skunk._
import skunk.implicits._

trait ObservationService[F[_]] {
  import ObservationService._

  def insertObservation(
    programId:     Program.Id,
    name:          Option[NonEmptyString] = NonEmptyString.unapply("Untitled Observation"),
    existence:     Existence              = Existence.Present,
    status:        ObsStatus              = ObsStatus.New,
    activeState:   ObsActiveStatus        = ObsActiveStatus.Active,
    constraintSet: ConstraintSet          = ConstraintSetInput.NominalConstraints
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
    (programId:     Program.Id,
     name:          Option[NonEmptyString],
     existence:     Existence,
     status:        ObsStatus,
     activeState:   ObsActiveStatus,
     constraintSet: ConstraintSet
    ) => {
      val af = Statements.insertObservationAs(user, programId, name, existence, status, activeState, constraintSet)
      session.prepare(af.fragment.query(observation_id)).use { pq =>
        pq.option(af.argument).map {
          case Some(oid) => Success(oid)
          case None      => NotAuthorized(user)
        }
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
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(_.min)        ~
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(_.max)        ~
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.minHours) ~
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.maxHours)
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
      Program.Id                    ~
      Option[NonEmptyString]        ~
      Existence                     ~
      ObsStatus                     ~
      ObsActiveStatus               ~
      CloudExtinction               ~
      ImageQuality                  ~
      SkyBackground                 ~
      WaterVapor                    ~
      Option[AirMass.DecimalValue]  ~
      Option[AirMass.DecimalValue]  ~
      Option[HourAngle.DecimalHour] ~
      Option[HourAngle.DecimalHour]
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

}