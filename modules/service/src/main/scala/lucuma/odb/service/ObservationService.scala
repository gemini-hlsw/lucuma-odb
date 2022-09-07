// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.Ior
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.all.*
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Problem
import edu.gemini.grackle.Result
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.FocalPlane
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.ScienceMode
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.SpectroscopyCapabilities
import lucuma.core.enums.WaterVapor
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.AirMass.DecimalValue
import lucuma.core.model.ElevationRange.HourAngle.DecimalHour
import lucuma.core.model.GuestRole
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.ServiceRole
import lucuma.core.model.StandardRole.*
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.data.Nullable
import lucuma.odb.data.Nullable.Absent
import lucuma.odb.data.Nullable.NonNull
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.Tag
import lucuma.odb.data.Timestamp
import lucuma.odb.graphql.input.AirMassRangeInput
import lucuma.odb.graphql.input.ConstraintSetInput
import lucuma.odb.graphql.input.ElevationRangeInput
import lucuma.odb.graphql.input.HourAngleRangeInput
import lucuma.odb.graphql.input.ObservationPropertiesInput
import lucuma.odb.graphql.input.PosAngleConstraintInput
import lucuma.odb.graphql.input.ScienceRequirementsInput
import lucuma.odb.graphql.input.SpectroscopyScienceRequirementsInput
import lucuma.odb.graphql.input.TargetEnvironmentInput
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.exception.PostgresErrorException
import skunk.implicits.*

trait ObservationService[F[_]] {
  import ObservationService._

  def createObservation(
    programId: Program.Id,
    SET:       ObservationPropertiesInput
  ): F[Result[Observation.Id]]

  def selectObservations(
    which: AppliedFragment
  ): F[List[Observation.Id]]

  def updateObservations(
    SET:   ObservationPropertiesInput,
    which: AppliedFragment
  ): F[Result[List[Observation.Id]]]

}


object ObservationService {

  final case class DatabaseConstraint(
    constraint: String,
    message:    String
  )

  val MissingAirMassConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "air_mass_neither_or_both",
      "airMass constraint requires both min and max values."
    )

  val MissingHourAngleConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "hour_angle_neither_or_both",
      "hourAngle constraint requires both minHours and maxHours."
    )

  val BothExplicitCoordinatesConstraint: DatabaseConstraint =
    DatabaseConstraint(
      "explicit_base_neither_or_both",
      "explicitBase requires both ra and dec"
    )

  def GenericConstraintViolationMessage(m: String): String =
    s"Database constraint violation produced by input: $m"

  val DatabaseConstraints: List[DatabaseConstraint] =
    List(
      MissingAirMassConstraint,
      MissingHourAngleConstraint,
      BothExplicitCoordinatesConstraint
    )

  def constraintViolationMessage(ex: PostgresErrorException): String =
    DatabaseConstraints
      .find { dc => ex.message.contains(dc.constraint) }
      .map(_.message)
      .getOrElse(GenericConstraintViolationMessage(ex.message))

  def fromSessionAndUser[F[_]: Sync: Trace](
    session: Session[F],
    user:    User
  ): ObservationService[F] =
    new ObservationService[F] {

      override def createObservation(
        programId:   Program.Id,
        SET:         ObservationPropertiesInput
      ): F[Result[Observation.Id]] =
        Trace[F].span("createObservation") {
          Statements
            .insertObservationAs(user, programId, SET)
            .flatTraverse { af =>
              session.prepare(af.fragment.query(observation_id)).use { pq =>
                pq.option(af.argument).map {
                  case Some(oid) => Result(oid)
                  case None      => Result.failure(s"User ${user.id} is not authorized to perform this action.")
                }
              }
            }
        }

      override def selectObservations(
        which: AppliedFragment
      ): F[List[Observation.Id]] =
        session.prepare(which.fragment.query(observation_id)).use { pq =>
          pq.stream(which.argument, chunkSize = 1024).compile.toList
        }

      override def updateObservations(
        SET:   ObservationPropertiesInput,
        which: AppliedFragment
      ): F[Result[List[Observation.Id]]] =
        Statements.updateObservations(SET, which).traverse { af =>
          session.prepare(af.fragment.query(observation_id)).use { pq =>
            pq.stream(af.argument, chunkSize = 1024).compile.toList
          }
        }.recoverWith {
           case SqlState.CheckViolation(ex) =>
             Result.failure(constraintViolationMessage(ex)).pure[F]
         }
    }


  object Statements {

    import ProgramService.Statements.whereUserAccess

    def insertObservationAs(
      user:      User,
      programId: Program.Id,
      SET:       ObservationPropertiesInput
    ): Result[AppliedFragment] =
      for {
        eb <- SET.targetEnvironment.flatMap(_.explicitBase.toOption).flatTraverse(_.create)
        cs <- SET.constraintSet.traverse(_.create)
      } yield
        insertObservationAs(
          user,
          programId,
          SET.subtitle.toOption,
          SET.existence.getOrElse(Existence.Default),
          SET.status.getOrElse(ObsStatus.New),
          SET.activeStatus.getOrElse(ObsActiveStatus.Active),
          SET.visualizationTime.toOption,
          SET.posAngleConstraint.flatMap(_.mode).getOrElse(PosAngleConstraintMode.Unbounded),
          SET.posAngleConstraint.flatMap(_.angle).getOrElse(Angle.Angle0),
          eb,
          cs.getOrElse(ConstraintSetInput.NominalConstraints),
          SET.scienceRequirements
        )

    def insertObservationAs(
      user:                User,
      programId:           Program.Id,
      subtitle:            Option[NonEmptyString],
      existence:           Existence,
      status:              ObsStatus,
      activeState:         ObsActiveStatus,
      visualizationTime:   Option[Timestamp],
      posAngleConsMode:    PosAngleConstraintMode,
      posAngle:            Angle,
      explicitBase:        Option[Coordinates],
      constraintSet:       ConstraintSet,
      scienceRequirements: Option[ScienceRequirementsInput]
    ): AppliedFragment = {

      val insert: AppliedFragment = {
        val spectroscopy: Option[SpectroscopyScienceRequirementsInput] =
          scienceRequirements.flatMap(_.spectroscopy)

        InsertObservation.apply(
          programId    ~
           subtitle    ~
           existence   ~
           status      ~
           activeState ~
           visualizationTime             ~
           posAngleConsMode              ~
           posAngle                      ~
           explicitBase.map(_.ra)        ~
           explicitBase.map(_.dec)       ~
           constraintSet.cloudExtinction ~
           constraintSet.imageQuality    ~
           constraintSet.skyBackground   ~
           constraintSet.waterVapor      ~
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.min.value)) ~  // TODO: fix in core
           ElevationRange.airMass.getOption(constraintSet.elevationRange).map(am => PosBigDecimal.unsafeFrom(am.max.value)) ~
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.minHours.value)                           ~
           ElevationRange.hourAngle.getOption(constraintSet.elevationRange).map(_.maxHours.value)                           ~
           scienceRequirements.flatMap(_.mode).getOrElse(ScienceMode.Spectroscopy)  ~
           spectroscopy.flatMap(_.wavelength.toOption)                              ~
           spectroscopy.flatMap(_.resolution.toOption)                              ~
           spectroscopy.flatMap(_.signalToNoise.toOption)                           ~
           spectroscopy.flatMap(_.signalToNoiseAt.toOption)                         ~
           spectroscopy.flatMap(_.wavelengthCoverage.toOption)                      ~
           spectroscopy.flatMap(_.focalPlane.toOption)                              ~
           spectroscopy.flatMap(_.focalPlaneAngle.toOption)                         ~
           spectroscopy.flatMap(_.capability.toOption)
        )
      }

      val returning: AppliedFragment =
        void"RETURNING c_observation_id"

      // done!
      insert |+| whereUserAccess(user, programId) |+| returning

    }

    val InsertObservation: Fragment[
      Program.Id             ~
      Option[NonEmptyString] ~
      Existence              ~
      ObsStatus              ~
      ObsActiveStatus        ~
      Option[Timestamp]      ~
      PosAngleConstraintMode ~
      Angle                  ~
      Option[RightAscension] ~
      Option[Declination]    ~
      CloudExtinction        ~
      ImageQuality           ~
      SkyBackground          ~
      WaterVapor             ~
      Option[PosBigDecimal]  ~
      Option[PosBigDecimal]  ~
      Option[BigDecimal]     ~
      Option[BigDecimal]     ~
      ScienceMode            ~
      Option[Wavelength]     ~
      Option[PosInt]         ~
      Option[PosBigDecimal]  ~
      Option[Wavelength]     ~
      Option[Wavelength]     ~
      Option[FocalPlane]     ~
      Option[Angle]          ~
      Option[SpectroscopyCapabilities]
    ] =
      sql"""
        INSERT INTO t_observation (
          c_program_id,
          c_subtitle,
          c_existence,
          c_status,
          c_active_status,
          c_visualization_time,
          c_pac_mode,
          c_pac_angle,
          c_explicit_ra,
          c_explicit_dec,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max,
          c_science_mode,
          c_spec_wavelength,
          c_spec_resolution,
          c_spec_signal_to_noise,
          c_spec_signal_to_noise_at,
          c_spec_wavelength_coverage,
          c_spec_focal_plane,
          c_spec_focal_plane_angle,
          c_spec_capability
        )
        SELECT
          $program_id,
          ${text_nonempty.opt},
          $existence,
          $obs_status,
          $obs_active_status,
          ${data_timestamp.opt},
          $pac_mode,
          $angle_µas,
          ${right_ascension.opt},
          ${declination.opt},
          $cloud_extinction,
          $image_quality,
          $sky_background,
          $water_vapor,
          ${air_mass_range_value.opt},
          ${air_mass_range_value.opt},
          ${hour_angle_range_value.opt},
          ${hour_angle_range_value.opt},
          $science_mode,
          ${wavelength_pm.opt},
          ${pos_int.opt},
          ${signal_to_noise.opt},
          ${wavelength_pm.opt},
          ${wavelength_pm.opt},
          ${focal_plane.opt},
          ${angle_µas.opt},
          ${spectroscopy_capabilities.opt}
      """

    def posAngleConstraintUpdates(in: PosAngleConstraintInput): List[AppliedFragment] = {

      val upMode  = sql"c_pac_mode  = $pac_mode"
      val upAngle = sql"c_pac_angle = $angle_µas"

      in.mode.map(upMode).toList ++ in.angle.map(upAngle).toList
    }

    def explicitBaseUpdates(in: TargetEnvironmentInput): Result[List[AppliedFragment]] = {

      val upRa  = sql"c_explicit_ra = ${right_ascension.opt}"
      val upDec = sql"c_explicit_dec = ${declination.opt}"

      in.explicitBase match {
        case Nullable.Null   => Result(List(upRa(none), upDec(none)))
        case Nullable.Absent => Result(Nil)
        case NonNull(value)  =>
          (value.ra.map(r => upRa(r.some)).toList ++ value.dec.map(d => upDec(d.some)).toList) match {
            case Nil => Result.failure("At least one of ra or dec must be specified for an edit")
            case lst => Result(lst)
          }
      }
    }

    def elevationRangeUpdates(in: ElevationRangeInput): Result[List[AppliedFragment]] = {
      val upAirMassMin   = sql"c_air_mass_min = ${air_mass_range_value.opt}"
      val upAirMassMax   = sql"c_air_mass_max = ${air_mass_range_value.opt}"
      val upHourAngleMin = sql"c_hour_angle_min = ${hour_angle_range_value.opt}"
      val upHourAngleMax = sql"c_hour_angle_max = ${hour_angle_range_value.opt}"

      val airMass: List[AppliedFragment] =
        List(
          in.airMass.flatMap(_.minPosBigDecimal).map(v => upAirMassMin(v.some)),
          in.airMass.flatMap(_.maxPosBigDecimal).map(v => upAirMassMax(v.some))
        ).flattenOption

      val hourAngle: List[AppliedFragment] =
        List(
          in.hourAngle.flatMap(_.minBigDecimal).map(v => upHourAngleMin(v.some)),
          in.hourAngle.flatMap(_.maxBigDecimal).map(v => upHourAngleMax(v.some))
        ).flattenOption

      (airMass, hourAngle) match {
        case (Nil, Nil) => Result(List.empty[AppliedFragment])
        case (am, Nil)  => Result(upHourAngleMin(None) :: upHourAngleMax(None) :: am)
        case (Nil, ha)  => Result(upAirMassMin(None) :: upAirMassMax(None) :: ha)
        case (_, _)     => Result.failure("Only one of airMass or hourAngle may be specified.")
      }
    }

    def constraintSetUpdates(in: ConstraintSetInput): Result[List[AppliedFragment]] = {
      val upCloud = sql"c_cloud_extinction = $cloud_extinction"
      val upImage = sql"c_image_quality = $image_quality"
      val upSky   = sql"c_sky_background = $sky_background"
      val upWater = sql"c_water_vapor = $water_vapor"

      val ups: List[AppliedFragment] =
        List(
          in.cloudExtinction.map(upCloud),
          in.imageQuality.map(upImage),
          in.skyBackground.map(upSky),
          in.waterVapor.map(upWater)
        ).flattenOption

      in.elevationRange
        .toList
        .flatTraverse(elevationRangeUpdates)
        .map(_ ++ ups)
    }

    def updates(SET: ObservationPropertiesInput): Result[Option[NonEmptyList[AppliedFragment]]] = {
      val upExistence         = sql"c_existence = $existence"
      val upSubtitle          = sql"c_subtitle = ${text_nonempty.opt}"
      val upStatus            = sql"c_status = $obs_status"
      val upActive            = sql"c_active_status = $obs_active_status"
      val upVisualizationTime = sql"c_visualization_time = ${data_timestamp.opt}"

      val ups: List[AppliedFragment] =
        List(
          SET.existence.map(upExistence),
          SET.subtitle match {
            case Nullable.Null  => Some(upSubtitle(None))
            case Absent         => None
            case NonNull(value) => Some(upSubtitle(Some(value)))
          },
          SET.status.map(upStatus),
          SET.activeStatus.map(upActive),
          SET.visualizationTime match {
            case Nullable.Null  => Some(upVisualizationTime(None))
            case Absent         => None
            case NonNull(value) => Some(upVisualizationTime(Some(value)))
          }
        ).flatten

      val posAngleConstraint: List[AppliedFragment] =
        SET.posAngleConstraint
           .toList
           .flatMap(posAngleConstraintUpdates)

      val explicitBase: Result[List[AppliedFragment]] =
        SET.targetEnvironment
           .toList
           .flatTraverse(explicitBaseUpdates)

      val constraintSet: Result[List[AppliedFragment]] =
        SET.constraintSet
           .toList
           .flatTraverse(constraintSetUpdates)

      (explicitBase, constraintSet).mapN { (eb, cs) =>
        NonEmptyList.fromList(eb ++ cs ++ ups ++ posAngleConstraint)
      }
    }

    def updateObservations(
      SET:   ObservationPropertiesInput,
      which: AppliedFragment
    ): Result[AppliedFragment] = {

      def update(us: NonEmptyList[AppliedFragment]): AppliedFragment =
        void"UPDATE t_observation "                                              |+|
          void"SET " |+| us.intercalate(void", ") |+| void" "                    |+|
          void"WHERE t_observation.c_observation_id IN (" |+| which |+| void") " |+|
          void"RETURNING t_observation.c_observation_id"

      updates(SET).map(_.fold(which)(update))

    }

  }


}