// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.Order.*
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.text.utf8
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.*
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.ags
import lucuma.ags.AgsPosition
import lucuma.ags.GuideProbe
import lucuma.ags.GuideStarCandidate
import lucuma.ags.*
import lucuma.catalog.votable.*
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.Site
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos.probeArm
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.HourAngle
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.client.ItcClient
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.json.all.query.given
import lucuma.odb.json.target
import lucuma.odb.logic.Generator
import lucuma.odb.logic.PlannedTimeCalculator
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.util.Codecs.*
import org.http4s.Header
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.client.Client
import skunk.AppliedFragment
import skunk.Decoder
import skunk.implicits.*

import java.time.temporal.ChronoUnit

import Services.Syntax.*

trait GuideService[F[_]] {
  import GuideService.AvailabilityPeriod
  import GuideService.Error
  import GuideService.GuideEnvironment

  def getGuideEnvironment(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(using
    NoTransaction[F]
  ): F[Either[Error, List[GuideEnvironment]]]

  def getGuideAvailability(pid: Program.Id, oid: Observation.Id, start: Timestamp, end: Timestamp)(using
    NoTransaction[F]
  ): F[Either[Error, List[AvailabilityPeriod]]]
}

object GuideService {
  // if any science target or guide star candidate moves more than this many milliarcseconds,
  // we consider it to potentially invalidate the availability.
  val invalidThreshold = 100.0

  given Order[Angle] = Angle.AngleOrder

  case class GuideTarget(probe: GuideProbe, target: Target)

  object GuideTarget {
    given Encoder[GuideTarget] =
      Encoder.instance { gt =>
        Json.obj(
          "probe"         -> gt.probe.asJson,
          "name"          -> gt.target.name.asJson,
          "sourceProfile" -> gt.target.sourceProfile.asJson,
          target.query.siderealOrNonJson(gt.target)
        )
      }
  }

  case class GuideEnvironment(posAngle: Angle, guideTargets: List[GuideTarget])

  object GuideEnvironment {
    given Encoder[GuideEnvironment] = deriveEncoder
  }

  case class AvailabilityPeriod(
    start:     Timestamp,
    end:       Timestamp,
    posAngles: List[Angle]
  )

  object AvailabilityPeriod {
    given Encoder[AvailabilityPeriod] = deriveEncoder
  }

  private case class AvailabilityList private (periods: List[AvailabilityPeriod]) {
    def add(ap: AvailabilityPeriod): AvailabilityList =
      periods match
        case h :: t if h.end === ap.start && h.posAngles === ap.posAngles => AvailabilityList(h.copy(end = ap.end) :: t)
        case _                                                            => AvailabilityList(ap :: periods)

    def toList: List[AvailabilityPeriod] = periods.reverse
  }

  private object AvailabilityList {
    def empty: AvailabilityList = AvailabilityList(List.empty)
  }

  sealed trait Error {
    def format: String
  }

  object Error {
    case class GeneralError(error: String) extends Error {
      val format: String = error
    }

    case class GeneratorError(
      error: Generator.Error
    ) extends Error {
      def format: String = error.format
    }

    case class GaiaError(error: String) extends Error {
      val format: String = s"Error calling Gaia: '$error'"
    }
  }

  private case class ObservationInfo(
    id:                 Observation.Id,
    constraints:        ConstraintSet,
    posAngleConstraint: PosAngleConstraint,
    optWavelength:      Option[Wavelength],
    explicitBase:       Option[Coordinates]
  ) {
    def wavelength: Either[Error, Wavelength] =
      optWavelength.toRight(Error.GeneralError(s"No wavelength defined for observation $id."))
  }

  private case class GeneratorInfo(
    digest: ExecutionDigest,
    params: GeneratorParams,
    hash:   Md5Hash
  ) {
    val plannedTime                          = digest.fullPlannedTime.sum
    val offsets                              = NonEmptyList.fromFoldable(digest.science.offsets.union(digest.acquisition.offsets))
    val (site, agsParams): (Site, AgsParams) = params match
      case GeneratorParams.GmosNorthLongSlit(_, mode) =>
        (Site.GN, AgsParams.GmosAgsParams(mode.fpu.asLeft.some, PortDisposition.Side))
      case GeneratorParams.GmosSouthLongSlit(_, mode) =>
        (Site.GS, AgsParams.GmosAgsParams(mode.fpu.asRight.some, PortDisposition.Side))

  }

  def instantiate[F[_]: Concurrent](
    httpClient:            Client[F],
    itcClient:             ItcClient[F],
    commitHash:            CommitHash,
    plannedTimeCalculator: PlannedTimeCalculator.ForInstrumentMode
  )(using Services[F]): GuideService[F] =
    new GuideService[F] {

      def getAsterism(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F]
      ): F[Either[Error, NonEmptyList[Target]]] =
        asterismService
          .getAsterism(pid, oid)
          .map(l =>
            NonEmptyList
              .fromList(
                l.map(_._2)
              )
              .toRight(Error.GeneralError(s"No targets have been defined for observation $oid."))
          )

      def getObservationInfo(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F]
      ): F[Either[Error, ObservationInfo]] = {
        val af = Statements.getObservationInfo(user, pid, oid)
        session
          .prepareR(
            af.fragment.query(Decoders.obsInfoDecoder)
          )
          .use(
            _.option(af.argument).map(_.toRight(Error.GeneralError(s"Observation $oid not found.")))
          )
      }

      def getGaiaQuery(
        oid:             Observation.Id,
        start:           Timestamp,
        end:             Timestamp,
        tracking:        ObjectTracking,
        shapeConstraint: ShapeExpression,
        wavelength:      Wavelength,
        constraints:     ConstraintSet
      ): Either[Error, ADQLQuery] =
        (tracking.at(start.toInstant), tracking.at(end.toInstant))
          .mapN { (a, b) =>
            val brightnessConstraints = gaiaBrightnessConstraints(constraints, GuideSpeed.Slow, wavelength)
            // Make a query based on two coordinates of the base of an asterism over a year
            CoordinatesRangeQueryByADQL(
              NonEmptyList.of(a.value, b.value),
              shapeConstraint,
              brightnessConstraints.some
            )
          }
          .toRight(
            Error.GeneralError(
              s"Unable to get tracking information for asterism for observation $oid."
            )
          )

      def callGaia(
        oid:   Observation.Id,
        query: ADQLQuery
      ): F[Either[Error, List[GuideStarCandidate]]] = {
        val MaxTargets                     = 100
        given catalog: CatalogAdapter.Gaia = CatalogAdapter.Gaia3Lite
        given ci: ADQLInterpreter          = ADQLInterpreter.nTarget(MaxTargets)
        val request                        = Request[F](Method.GET,
                                 CatalogSearch.gaiaSearchUri(query),
                                 headers = Headers(("x-requested-with", "XMLHttpRequest"))
        )
        httpClient
          .stream(request)
          .flatMap(
            _.body
              .through(utf8.decode)
              .through(CatalogSearch.guideStars[F](CatalogAdapter.Gaia3Lite))
          )
          .compile
          .toList
          .map(_.collect { case Right(s) => GuideStarCandidate.siderealTarget.get(s) }.asRight)
          .handleError(e => Error.GaiaError(e.getMessage()).asLeft)
      }

      def getCandidates(
        oid:         Observation.Id,
        start:       Timestamp,
        end:         Timestamp,
        tracking:    ObjectTracking,
        wavelength:  Wavelength,
        constraints: ConstraintSet
      ): F[Either[Error, List[GuideStarCandidate]]] =
        (for {
          query      <- EitherT.fromEither(
                          getGaiaQuery(oid, start, end, tracking, probeArm.candidatesArea, wavelength, constraints)
                        )
          candidates <- EitherT(callGaia(oid, query))
        } yield candidates).value

      extension [D](steps: NonEmptyList[Step[D]])
        def offsets: List[Offset] = steps.collect { case Step(_, _, StepConfig.Science(offset, _), _, _, _) =>
          offset
        }

      extension (target: Target)
        def masy: Double =
          Target.siderealTracking.getOption(target).map(_.masy).getOrElse(0.0)

      extension (sidereal: SiderealTracking)
        def masy: Double =
          sidereal.properMotion
            .map(pm => (pm.ra.masy.value.pow(2) + pm.dec.masy.value.pow(2)).toReal.sqrt.toDouble)
            .getOrElse(0.0)

      extension (usable: List[AgsAnalysis.Usable])
        def toGuideEnvironments: List[GuideEnvironment] = usable.map { ags =>
          val target = GuideStarCandidate.siderealTarget.reverseGet(ags.target)
          GuideEnvironment(ags.vignetting.head._1, List(GuideTarget(ags.guideProbe, target)))
        }

      def getGeneratorInfo(
        pid: Program.Id,
        oid: Observation.Id
      ): F[Either[Error, GeneratorInfo]] =
        generator(commitHash, itcClient, plannedTimeCalculator)
          .digestWithParamsAndHash(pid, oid)
          .map {
            _.leftMap(Error.GeneratorError(_))
              .map((d, p, h) => GeneratorInfo(d, p, h))
          }

      def getPositions(
        oid:                Observation.Id,
        site:               Site,
        posAngleConstraint: PosAngleConstraint,
        offsets:            Option[NonEmptyList[Offset]],
        tracking:           ObjectTracking,
        obsTime:            Timestamp,
        obsDuration:        TimeSpan
      ): Either[Error, NonEmptyList[AgsPosition]] = {
        val angles     =
          posAngleConstraint
            .anglesToTestAt(site, tracking, obsTime.toInstant, obsDuration.toDuration)
            .map(_.sorted)
        val newOffsets = offsets.getOrElse(NonEmptyList.of(Offset.Zero))

        angles
          .map(toTest =>
            for {
              pa  <- toTest
              off <- newOffsets
            } yield AgsPosition(pa, off)
          )
          .toRight(Error.GeneralError(s"No angles to test for guide target candidates for observation $oid."))
      }

      private val AllAngles =
        NonEmptyList.fromListUnsafe(
          (0 until 360 by 10).map(a => Angle.fromDoubleDegrees(a.toDouble)).toList
        )

      def getAllAnglePositions(offsets: Option[NonEmptyList[Offset]]): NonEmptyList[AgsPosition] =
        for {
          pa  <- AllAngles
          off <- offsets.getOrElse(NonEmptyList.of(Offset.Zero))
        } yield AgsPosition(pa, off)

      def processCandidates(
        obsInfo:       ObservationInfo,
        wavelength:    Wavelength,
        genInfo:       GeneratorInfo,
        baseCoords:    Coordinates,
        scienceCoords: List[Coordinates],
        positions:     NonEmptyList[AgsPosition],
        candidates:    List[GuideStarCandidate]
      ): List[AgsAnalysis.Usable] =
        Ags
          .agsAnalysis(obsInfo.constraints,
                       wavelength,
                       baseCoords,
                       scienceCoords,
                       positions,
                       genInfo.agsParams,
                       candidates
          )
          .sortUsablePositions
          .collect { case usable: AgsAnalysis.Usable => usable }

      def buildAvailability(
        start:      Timestamp,
        end:        Timestamp,
        obsInfo:    ObservationInfo,
        genInfo:    GeneratorInfo,
        wavelength: Wavelength,
        asterism:   NonEmptyList[Target],
        tracking:   ObjectTracking,
        candidates: List[GuideStarCandidate],
        positions:  NonEmptyList[AgsPosition]
      ): Either[Error, List[AvailabilityPeriod]] = {
        @scala.annotation.tailrec
        def go(startTime: Timestamp, accum: AvailabilityList): Either[Error, AvailabilityList] = {
          val period =
            buildAvailabilityPeriod(startTime, obsInfo, genInfo, wavelength, asterism, tracking, candidates, positions)
          period match
            case Left(error)                       => error.asLeft
            case Right(period) if period.end < end => go(period.end, accum.add(period))
            case Right(period)                     => accum.add(period.copy(end = end)).asRight
        }
        candidates match
          case Nil => List(AvailabilityPeriod(start, end, List.empty)).asRight
          case _   => go(start, AvailabilityList.empty).map(_.toList)
      }

      def buildAvailabilityPeriod(
        start:      Timestamp,
        obsInfo:    ObservationInfo,
        genInfo:    GeneratorInfo,
        wavelength: Wavelength,
        asterism:   NonEmptyList[Target],
        tracking:   ObjectTracking,
        candidates: List[GuideStarCandidate],
        positions:  NonEmptyList[AgsPosition]
      ): Either[Error, AvailabilityPeriod] =
        for {
          baseCoords      <- obsInfo.explicitBase
                               .orElse(
                                 tracking
                                   .at(start.toInstant)
                                   .map(_.value)
                               )
                               .toRight(
                                 Error.GeneralError(
                                   s"Unable to get coordinates for asterism in observation ${obsInfo.id}"
                                 )
                               )
          scienceCoords   <- asterism.toList
                               .traverse(t => ObjectTracking.fromTarget(t).at(start.toInstant).map(_.value))
                               .toRight(
                                 Error.GeneralError(
                                   s"Unable to get coordinates for science targets in observation ${obsInfo.id}"
                                 )
                               )
          scienceSpeed     = asterism.toList.map(_.masy).max
          candidatesAt     = candidates.map(_.at(start.toInstant))
          analysis         = Ags.agsAnalysis(obsInfo.constraints,
                                             wavelength,
                                             baseCoords,
                                             scienceCoords,
                                             positions,
                                             genInfo.agsParams,
                                             candidatesAt
                             )
          // we don't care about specific targets, just their angles and speeds.
          // At this point, each candidate only has a single vignetting
          candidateSpeeds  = analysis.collect { case u: AgsAnalysis.Usable => (u.vignetting.head._1, u.target.tracking.masy) }
          // At this point, each Usable has a single vignetting
          angleGroups      = candidateSpeeds.groupMap(_._1)(_._2)
          // we want the slowest speed at each angle, because that will be the one that is available longest,
          // maintaing the same list of angles.
          slowestPerAngle  = angleGroups.toList.map((angle, speeds) => (angle, speeds.min))
          // Now we want to know which angle will be invalid first. If there are no available candidates,
          // we'll invalidate on the science targets.
          fastestAngle     = slowestPerAngle.maxByOption((_, speed) => speed).fold(0.0)(_._2)
          // And now, which is faster, the science target or the candidates?
          fastest          = scala.math.max(scienceSpeed, fastestAngle)
          // This is approximate, but so is the invalidThreshold...
          // These should always be big values (until nonsidereal), but if it is 0, we'd go infinite.
          daysTilInvalid   = scala.math.max((invalidThreshold / fastest * 365).intValue, 1)
          invalidDate      = Timestamp
                               .fromInstantTruncated(start.toInstant.plus(daysTilInvalid, ChronoUnit.DAYS))
                               .getOrElse(Timestamp.Max)
        } yield AvailabilityPeriod(start, invalidDate, slowestPerAngle.map(_._1).sorted)

      override def getGuideEnvironment(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(using
        NoTransaction[F]
      ): F[Either[Error, List[GuideEnvironment]]] =
        (for {
          obsInfo       <- EitherT(getObservationInfo(pid, oid))
          wavelength    <- EitherT.fromEither(obsInfo.wavelength)
          asterism      <- EitherT(getAsterism(pid, oid))
          genInfo       <- EitherT(getGeneratorInfo(pid, oid))
          tracking       = ObjectTracking.fromAsterism(asterism)
          visitEnd      <- EitherT.fromEither(
                             obsTime
                               .plusMicrosOption(genInfo.plannedTime.toMicroseconds)
                               .toRight(Error.GeneralError("Visit end time out of range"))
                           )
          candidates    <- EitherT(
                            getCandidates(oid, obsTime, visitEnd, tracking, wavelength, obsInfo.constraints)
                          ).map(_.map(_.at(obsTime.toInstant)))
          baseCoords    <- EitherT.fromEither(
                             obsInfo.explicitBase
                               .orElse(
                                 tracking
                                   .at(obsTime.toInstant)
                                   .map(_.value)
                               )
                               .toRight(
                                 Error.GeneralError(
                                   s"Unable to get coordinates for asterism in observation $oid"
                                 )
                               )
                           )
          scienceCoords <- EitherT.fromEither(
                             asterism.toList
                               .traverse(t => ObjectTracking.fromTarget(t).at(obsTime.toInstant).map(_.value))
                               .toRight(
                                 Error.GeneralError(
                                   s"Unable to get coordinates for science targets in observation $oid"
                                 )
                               )
                           )
          positions     <- EitherT.fromEither(
                             getPositions(oid,
                                          genInfo.site,
                                          obsInfo.posAngleConstraint,
                                          genInfo.offsets,
                                          tracking,
                                          obsTime,
                                          genInfo.plannedTime
                             )
                           )
          usable         = processCandidates(obsInfo, wavelength, genInfo, baseCoords, scienceCoords, positions, candidates)
        } yield usable.toGuideEnvironments.toList).value

      override def getGuideAvailability(pid: Program.Id, oid: Observation.Id, start: Timestamp, end: Timestamp)(using
        NoTransaction[F]
      ): F[Either[Error, List[AvailabilityPeriod]]] =
        (for {
          _            <- EitherT.fromEither(
                            if (start < end) ().asRight
                            else Error.GeneralError("Start time must be prior to end time for guide star availability").asLeft
                          )
          obsInfo      <- EitherT(getObservationInfo(pid, oid))
          wavelength   <- EitherT.fromEither(obsInfo.wavelength)
          asterism     <- EitherT(getAsterism(pid, oid))
          genInfo      <- EitherT(getGeneratorInfo(pid, oid))
          tracking      = ObjectTracking.fromAsterism(asterism)
          candidates   <- EitherT(getCandidates(oid, start, end, tracking, wavelength, obsInfo.constraints))
          positions     = getAllAnglePositions(genInfo.offsets)
          availability <-
            EitherT.fromEither(
              buildAvailability(start, end, obsInfo, genInfo, wavelength, asterism, tracking, candidates, positions)
            )
        } yield availability).value
    }

  object Statements {
    import ProgramService.Statements.andWhereUserAccess

    def getObservationInfo(user: User, pid: Program.Id, oid: Observation.Id): AppliedFragment =
      sql"""
        select
          obs.c_observation_id,
          obs.c_cloud_extinction,
          obs.c_image_quality,
          obs.c_sky_background,
          obs.c_water_vapor,
          obs.c_air_mass_min,
          obs.c_air_mass_max,
          obs.c_hour_angle_min,
          obs.c_hour_angle_max,
          obs.c_pac_mode,
          obs.c_pac_angle,
          obs.c_spec_wavelength,
          obs.c_explicit_ra,
          obs.c_explicit_dec
        from t_observation obs
        where obs.c_program_id = $program_id
          and obs.c_observation_id = $observation_id
      """.apply(pid, oid) |+| andWhereUserAccess(user, pid)
  }

  private object Decoders {

    val obsInfoDecoder: Decoder[ObservationInfo] =
      (observation_id *:
        cloud_extinction *:
        image_quality *:
        sky_background *:
        water_vapor *:
        air_mass_range_value.opt *:
        air_mass_range_value.opt *:
        hour_angle_range_value.opt *:
        hour_angle_range_value.opt *:
        pac_mode *:
        angle_µas *:
        wavelength_pm.opt *:
        right_ascension.opt *:
        declination.opt).emap {
        case (id, cloud, image, sky, water, amMin, amMax, haMin, haMax, mode, angle, wavelength, ra, dec) =>
          val paConstraint: PosAngleConstraint = mode match
            case PosAngleConstraintMode.Unbounded           => PosAngleConstraint.Unbounded
            case PosAngleConstraintMode.Fixed               => PosAngleConstraint.Fixed(angle)
            case PosAngleConstraintMode.AllowFlip           => PosAngleConstraint.AllowFlip(angle)
            case PosAngleConstraintMode.AverageParallactic  => PosAngleConstraint.AverageParallactic
            case PosAngleConstraintMode.ParallacticOverride =>
              PosAngleConstraint.ParallacticOverride(angle)

          val elevRange =
            (for {
              min  <- amMin
              max  <- amMax
              aMin <- AirMass.DecimalValue.from(min.value).toOption
              aMax <- AirMass.DecimalValue.from(max.value).toOption
              am   <- AirMass.fromOrderedDecimalValues.getOption((aMin, aMax))
            } yield am)
              .orElse(
                for {
                  min  <- haMin
                  max  <- haMax
                  hMin <- HourAngle.DecimalHour.from(min).toOption
                  hMax <- HourAngle.DecimalHour.from(max).toOption
                  ha   <- HourAngle.fromOrderedDecimalHours.getOption((hMin, hMax))
                } yield ha
              )
              .toRight(s"Invalid elevation range in observation $id.")

          val explicitBase: Option[Coordinates] =
            (ra, dec).mapN(Coordinates(_, _))

          elevRange.map(elev =>
            ObservationInfo(id, ConstraintSet(image, cloud, sky, water, elev), paConstraint, wavelength, explicitBase)
          )
      }
  }
}
