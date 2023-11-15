// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.Order.*
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
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
import lucuma.core.util.TimestampInterval
import lucuma.itc.client.ItcClient
import lucuma.odb.data.ContiguousTimestampMap
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
import scala.collection.immutable.SortedMap

import Services.Syntax.*

trait GuideService[F[_]] {
  import GuideService.AvailabilityPeriod
  import GuideService.Error
  import GuideService.GuideEnvironment

  def getGuideEnvironment(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(using
    NoTransaction[F]
  ): F[Either[Error, List[GuideEnvironment]]]

  def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(using
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
    period:    TimestampInterval,
    posAngles: List[Angle]
  )

  object AvailabilityPeriod {
    def apply(start: Timestamp, end: Timestamp, posAngles: List[Angle]): AvailabilityPeriod =
      AvailabilityPeriod(TimestampInterval.between(start, end), posAngles)

    def fromTuple(tuple: (TimestampInterval, List[Angle])): AvailabilityPeriod =
      AvailabilityPeriod(tuple._1, tuple._2)

    given Encoder[AvailabilityPeriod] =
      Encoder.instance { ap =>
        Json.obj(
          "start"     -> ap.period.start.asJson,
          "end"       -> ap.period.end.asJson,
          "posAngles" -> ap.posAngles.asJson
        )
      }
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
            // If caching is implemented for the guide star results, `ags.widestConstraints` should be
            // used for the brightness constraints.
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
              .collect { case Right(s) => GuideStarCandidate.siderealTarget.get(s)}
          )
          .compile
          .toList
          .map(_.asRight)
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
        def invalidDate(startDate: Timestamp): Timestamp = 
          Target.siderealTracking.getOption(target).map(_.invalidDate(startDate)).getOrElse(Timestamp.Max)

      extension (sidereal: SiderealTracking)
        def masy: Double =
          sidereal.properMotion
            .map(pm => (pm.ra.masy.value.pow(2) + pm.dec.masy.value.pow(2)).toReal.sqrt.toDouble)
            .getOrElse(0.0)
        def invalidDate(startDate: Timestamp): Timestamp = {
          val speed = masy
          if (speed === 0.0) Timestamp.Max
          else {
            // This is approximate, but so is the invalidThreshold...
            // These should always be big values (until nonsidereal), but if it is 0, we'd go infinite loop.
            val daysTilInvalid = scala.math.max((invalidThreshold / speed * 365).intValue, 1)
            Timestamp.fromInstantTruncated(startDate.toInstant.plus(daysTilInvalid, ChronoUnit.DAYS))
              .getOrElse(Timestamp.Max)
          }
        }

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
        angles:             NonEmptyList[Angle],
        offsets:            Option[NonEmptyList[Offset]],
      ): NonEmptyList[AgsPosition] = 
        for {
          pa  <- angles
          off <- offsets.getOrElse(NonEmptyList.of(Offset.Zero))
        } yield AgsPosition(pa, off)

      private val AllAngles =
        NonEmptyList.fromListUnsafe(
          (0 until 360 by 10).map(a => Angle.fromDoubleDegrees(a.toDouble)).toList
        )

      def getAnglesForAvailability(posAngleConstraint: PosAngleConstraint): NonEmptyList[Angle] =
        posAngleConstraint match 
          case PosAngleConstraint.Fixed(a)               => NonEmptyList.of(a)
          case PosAngleConstraint.AllowFlip(a)           => NonEmptyList.of(a, a.flip)
          case PosAngleConstraint.ParallacticOverride(a) => NonEmptyList.of(a)
          case PosAngleConstraint.AverageParallactic     => AllAngles
          case PosAngleConstraint.Unbounded              => AllAngles
        
        
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
        candidates: List[GuideStarCandidate]
      ): Either[Error, List[AvailabilityPeriod]] = {
        val angles    = getAnglesForAvailability(obsInfo.posAngleConstraint)
        val positions = getPositions(angles, genInfo.offsets)

        @scala.annotation.tailrec
        def go(startTime: Timestamp, accum: ContiguousTimestampMap[List[Angle]]): Either[Error, ContiguousTimestampMap[List[Angle]]] = {
          val eap =
            buildAvailabilityPeriod(startTime, end, obsInfo, genInfo, wavelength, asterism, tracking, candidates, positions, angles)
          eap match
            case Left(error)                      => error.asLeft
            case Right(ap) if ap.period.end < end => go(ap.period.end, accum.unsafeAdd(ap.period, ap.posAngles))
            case Right(ap)                        => 
              val newPeriod = TimestampInterval.between(ap.period.start, end)
              accum.unsafeAdd(newPeriod, ap.posAngles).asRight
        }
        candidates match
          case Nil => List(AvailabilityPeriod(start, end, List.empty)).asRight
          case _   => go(start, ContiguousTimestampMap.empty[List[Angle]]).map(_.intervals.toList.map(AvailabilityPeriod.fromTuple))
      }

      def buildAvailabilityPeriod(
        start:      Timestamp,
        end:        Timestamp,
        obsInfo:    ObservationInfo,
        genInfo:    GeneratorInfo,
        wavelength: Wavelength,
        asterism:   NonEmptyList[Target],
        tracking:   ObjectTracking,
        candidates: List[GuideStarCandidate],
        positions:  NonEmptyList[AgsPosition],
        angles:     NonEmptyList[Angle]
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
          scienceCutoff    = asterism.map(_.invalidDate(start)).toList.min
          // we can stop testing candidates when all angles being tested have invalid dates that are farther
          // in the future than either the end time passed in, or a science candidate will be invalid
          endCutoff        = scienceCutoff.min(end)
          candidatesAt     = candidates.map(_.at(start.toInstant))
          angleMap         = getAvailabilityMap(
                               candidatesAt,
                               start,
                               endCutoff,
                               obsInfo.constraints,
                               wavelength,
                               baseCoords,
                               scienceCoords,
                               angles,
                               positions,
                               genInfo.agsParams
                             )
          candidateCutoff  = angleMap.values.minOption.getOrElse(Timestamp.Max) 
          finalEnd         = endCutoff.min(candidateCutoff)
        } yield AvailabilityPeriod(start, finalEnd, angleMap.keys.toList)

      def getAvailabilityMap(
        candidates: List[GuideStarCandidate],
        start:         Timestamp,
        endCutoff:     Timestamp, // The oldest time we need to satisfy to allow short cutting
        constraints:   ConstraintSet,
        wavelength:    Wavelength,
        baseCoords:    Coordinates,
        scienceCoords: List[Coordinates],
        angles:        NonEmptyList[Angle],
        positions:     NonEmptyList[AgsPosition],
        params:        AgsParams
      ): SortedMap[Angle, Timestamp] = {
        Stream.emits(candidates)
          .through(
            Ags.agsAnalysisStream(
              constraints,
              wavelength,
              baseCoords,
              scienceCoords,
              positions,
              params
            )
          )
          .collect { case u: AgsAnalysis.Usable => u }
          .scan(SortedMap.empty[Angle, Timestamp]){ (map, usable) => 
            val invalidDate = usable.target.tracking.invalidDate(start)
            val angle       = usable.vignetting.head._1
            // Always keep the oldest invalidation date for each angle
            map.updatedWith(angle)(_.fold(invalidDate)(_.max(invalidDate)).some)
          }
          // stop if/when we have all the angles and they all have expirations beyond the cutoff
          .takeThrough(map => map.size < angles.length || map.exists((_, t) => t <= endCutoff))
          .last
          .toList
          .headOption
          .flatten
          .getOrElse(SortedMap.empty)
      }

      override def getGuideEnvironment(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(using
        NoTransaction[F]
      ): F[Either[Error, List[GuideEnvironment]]] =
        (for {
          obsInfo       <- EitherT(getObservationInfo(pid, oid))
          wavelength    <- EitherT.fromEither(obsInfo.wavelength)
          asterism      <- EitherT(getAsterism(pid, oid))
          genInfo       <- EitherT(getGeneratorInfo(pid, oid))
          baseTracking   = obsInfo.explicitBase.fold(ObjectTracking.fromAsterism(asterism))(ObjectTracking.constant)
          visitEnd      <- EitherT.fromEither(
                             obsTime
                               .plusMicrosOption(genInfo.plannedTime.toMicroseconds)
                               .toRight(Error.GeneralError("Visit end time out of range"))
                           )
          candidates    <- EitherT(
                            getCandidates(oid, obsTime, visitEnd, baseTracking, wavelength, obsInfo.constraints)
                          ).map(_.map(_.at(obsTime.toInstant)))
          baseCoords    <- EitherT.fromEither(
                             baseTracking.at(obsTime.toInstant).map(_.value)
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
          angles        <- EitherT.fromEither(
                             obsInfo.posAngleConstraint
                              .anglesToTestAt(genInfo.site, baseTracking, obsTime.toInstant, genInfo.plannedTime.toDuration)
                              .toRight(Error.GeneralError(s"No angles to test for guide target candidates for observation $oid."))
                           )
          positions      = getPositions(angles, genInfo.offsets)
          usable         = processCandidates(obsInfo, wavelength, genInfo, baseCoords, scienceCoords, positions, candidates)
        } yield usable.toGuideEnvironments.toList).value

      override def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(using
        NoTransaction[F]
      ): F[Either[Error, List[AvailabilityPeriod]]] =
        (for {
          obsInfo      <- EitherT(getObservationInfo(pid, oid))
          start         = period.start
          end           = period.end
          wavelength   <- EitherT.fromEither(obsInfo.wavelength)
          asterism     <- EitherT(getAsterism(pid, oid))
          genInfo      <- EitherT(getGeneratorInfo(pid, oid))
          tracking      = ObjectTracking.fromAsterism(asterism)
          candidates   <- EitherT(getCandidates(oid, period.start, period.end, tracking, wavelength, obsInfo.constraints))
          availability <-
            EitherT.fromEither(
              buildAvailability(start, end, obsInfo, genInfo, wavelength, asterism, tracking, candidates)
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
