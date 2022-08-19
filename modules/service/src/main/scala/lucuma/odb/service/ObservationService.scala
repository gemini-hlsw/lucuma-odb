// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.{Ior, NonEmptyChain, NonEmptyList, ValidatedNec}
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.effect.Sync
import cats.syntax.all.*
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Problem
import edu.gemini.grackle.Result
import eu.timepit.refined.api.Refined.value
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CloudExtinction
import lucuma.core.enums.ImageQuality
import lucuma.core.enums.SkyBackground
import lucuma.core.enums.WaterVapor
import lucuma.core.math.{Coordinates, Declination, RightAscension}
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
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.odb.data.Tag
import lucuma.odb.graphql.input.{AirMassRangeInput, ConstraintSetInput, ElevationRangeInput, HourAngleRangeInput, ObservationPropertiesInput, TargetEnvironmentInput}
import lucuma.odb.util.Codecs.*
import natchez.Trace
import skunk.*
import skunk.exception.PostgresErrorException
import skunk.implicits.*

trait ObservationService[F[_]] {
  import ObservationService._

  def createObservation(
    programId:   Program.Id,
    SET:         ObservationPropertiesInput
  ): F[CreateResult]

  def updateObservations(
    SET:   ObservationPropertiesInput,
    which: AppliedFragment
  ): F[UpdateResult]

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

  sealed trait CreateResult
  object CreateResult {
    final case class BadInput(problems: NonEmptyChain[String]) extends CreateResult {
      def toResult[A]: Result[A] =
        Ior.left(problems.map(Problem(_)))
    }

    object BadInput {
      def apply(problem: String, problems: String*): CreateResult =
        BadInput(NonEmptyChain.of(problem, problems *))
    }


    final case class NotAuthorized(user: User)                 extends CreateResult {
      def toResult[A]: Result[A] =
        Result.failure(s"User ${user.id} is not authorized to perform this action.")
    }

    final case class Success(id: Observation.Id)               extends CreateResult
  }

  sealed trait UpdateResult

  object UpdateResult {
    final case class BadInput(problems: NonEmptyChain[String]) extends UpdateResult {
      def toResult[A]: Result[A] =
        Ior.left(problems.map(Problem(_)))
    }

    object BadInput {
      def apply(problem: String, problems: String*): UpdateResult =
        BadInput(NonEmptyChain.of(problem, problems*))
    }

    final case class Success(ids: List[Observation.Id])        extends UpdateResult
  }


  def fromSessionAndUser[F[_]: Sync: Trace](
    session: Session[F],
    user:    User
  ): ObservationService[F] =
    new ObservationService[F] {

      override def createObservation(
        programId:   Program.Id,
        SET:         ObservationPropertiesInput
      ): F[CreateResult] =
        Trace[F].span("createObservation") {
          Statements
            .insertObservationAs(user, programId, SET)
            .fold(
              ps => CreateResult.BadInput(ps).pure[F],
              af =>
                session.prepare(af.fragment.query(observation_id)).use { pq =>
                  pq.option(af.argument).map {
                    case Some(oid) => CreateResult.Success(oid)
                    case None      => CreateResult.NotAuthorized(user)
                  }
                }

            )
        }

      override def updateObservations(
        SET:   ObservationPropertiesInput,
        which: AppliedFragment
      ): F[UpdateResult] =
        Statements.updateObservations(SET, which) match {
          case Invalid(msg) =>
            UpdateResult.BadInput(msg).pure[F]

          case Valid(oaf) =>
            oaf.toList.flatTraverse { af =>
              session.prepare(af.fragment.query(observation_id)).use { pq =>
                pq.stream(af.argument, chunkSize = 1024).compile.toList
              }
            }.map(UpdateResult.Success(_))
             .recoverWith {
               case SqlState.CheckViolation(ex) =>
                 UpdateResult.BadInput(constraintViolationMessage(ex)).pure[F]
             }
        }
    }


  object Statements {

    import ProgramService.Statements.{
      existsUserAsPi,
      existsUserAsCoi,
      existsAllocationForPartner
    }

    def insertObservationAs(
      user:      User,
      programId: Program.Id,
      SET:       ObservationPropertiesInput
    ): ValidatedNec[String, AppliedFragment] =
      (SET.targetEnvironment.flatMap(_.explicitBase.toOption).flatTraverse(_.create),
       SET.constraintSet.traverse(_.create)
      ).mapN { (eb, cs) =>
        insertObservationAs(
          user,
          programId,
          SET.subtitle.toOption,
          SET.existence.getOrElse(Existence.Default),
          SET.status.getOrElse(ObsStatus.Default),
          SET.activeStatus.getOrElse(ObsActiveStatus.Default),
          eb,
          cs.getOrElse(ConstraintSetInput.NominalConstraints)
        )
      }

    def insertObservationAs(
      user:          User,
      programId:     Program.Id,
      subtitle:      Option[NonEmptyString],
      existence:     Existence,
      status:        ObsStatus,
      activeState:   ObsActiveStatus,
      explicitBase:  Option[Coordinates],
      constraintSet: ConstraintSet
    ): AppliedFragment = {

      val insert: AppliedFragment =
        InsertObservation.apply(
          programId    ~
           subtitle    ~
           existence   ~
           status      ~
           activeState ~
           explicitBase.map(_.ra)        ~
           explicitBase.map(_.dec)       ~
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
      Option[RightAscension] ~
      Option[Declination]    ~
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
          c_explicit_ra,
          c_explicit_dec,
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
          ${right_ascension.opt},
          ${declination.opt},
          $cloud_extinction,
          $image_quality,
          $sky_background,
          $water_vapor,
          ${air_mass_range_value.opt},
          ${air_mass_range_value.opt},
          ${hour_angle_range_value.opt},
          ${hour_angle_range_value.opt}
      """

    def explicitBaseUpdates(in: TargetEnvironmentInput): ValidatedNec[String, List[AppliedFragment]] = {

      val upRa  = sql"c_explicit_ra = ${right_ascension.opt}"
      val upDec = sql"c_explicit_dec = ${declination.opt}"

      in.explicitBase match {
        case Nullable.Null   => List(upRa(none), upDec(none)).validNec
        case Nullable.Absent => Nil.validNec
        case NonNull(value)  =>
          (value.ra.map(r => upRa(r.some)).toList ++ value.dec.map(d => upDec(d.some)).toList) match {
            case Nil => "At least one of ra or dec must be specified for an edit".invalidNec
            case lst => lst.validNec
          }
      }
    }

    def elevationRangeUpdates(in: ElevationRangeInput): ValidatedNec[String, List[AppliedFragment]] = {
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
        case (Nil, Nil) => List.empty[AppliedFragment].validNec
        case (am, Nil)  => (upHourAngleMin(None) :: upHourAngleMax(None) :: am).validNec
        case (Nil, ha)  => (upAirMassMin(None) :: upAirMassMax(None) :: ha).validNec
        case (_, _)     => "Only one of airMass or hourAngle may be specified.".invalidNec
      }
    }

    def constraintSetUpdates(in: ConstraintSetInput): ValidatedNec[String, List[AppliedFragment]] = {
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

    def updates(SET: ObservationPropertiesInput): ValidatedNec[String, Option[NonEmptyList[AppliedFragment]]] = {
      val upExistence = sql"c_existence = $existence"
      val upSubtitle  = sql"c_subtitle = ${text_nonempty.opt}"
      val upStatus    = sql"c_status = $obs_status"
      val upActive    = sql"c_active_status = $obs_active_status"

      val ups: List[AppliedFragment] =
        List(
          SET.existence.map(upExistence),
          SET.subtitle match {
            case Nullable.Null  => Some(upSubtitle(None))
            case Absent         => None
            case NonNull(value) => Some(upSubtitle(Some(value)))
          },
          SET.status.map(upStatus),
          SET.activeStatus.map(upActive)
        ).flatten

      val explicitBase: ValidatedNec[String, List[AppliedFragment]] =
        SET.targetEnvironment
           .toList
           .flatTraverse(explicitBaseUpdates)

      val constraintSet: ValidatedNec[String, List[AppliedFragment]] =
        SET.constraintSet
           .toList
           .flatTraverse(constraintSetUpdates)

      (explicitBase, constraintSet).mapN { (eb, cs) =>
        NonEmptyList.fromList(eb ++ cs ++ ups)
      }
    }

    def updateObservations(
      SET:   ObservationPropertiesInput,
      which: AppliedFragment
    ): ValidatedNec[String, Option[AppliedFragment]] =
      updates(SET).map {
        _.map { us =>
          void"UPDATE t_observation " |+|
            void"SET " |+| us.intercalate(void", ") |+| void" " |+|
            void"WHERE t_observation.c_observation_id IN (" |+| which |+| void")" |+|
            void"RETURNING t_observation.c_observation_id"
        }
      }

  }


}