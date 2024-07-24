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
import lucuma.ags.*
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.catalog.votable.*
import lucuma.core.enums.GuideProbe
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
import lucuma.itc.client.json.given
import lucuma.odb.data.ContiguousTimestampMap
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.json.all.query.given
import lucuma.odb.json.target
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculator
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.util.Codecs.*
import org.http4s.Header
import org.http4s.Headers
import org.http4s.Method
import org.http4s.Request
import org.http4s.client.Client
import skunk.*
import skunk.data.Arr
import skunk.implicits.*

import java.security.MessageDigest
import java.time.Duration
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

  // The longest availability period we will calculate.
  val maxAvailabilityPeriodDays = 200L
  val maxAvailabilityPeriod = TimeSpan.unsafeFromDuration(Duration.ofDays(maxAvailabilityPeriodDays))

  given Order[Angle] = Angle.AngleOrder

  case class GuideTarget(probe: GuideProbe, target: Target)

  object GuideTarget {
    given Encoder[GuideTarget] =
      Encoder.instance { gt =>
        Json.obj(
          "probe"         -> gt.probe.asJson,
          "name"          -> gt.target.name.asJson,
          "sourceProfile" -> gt.target.sourceProfile.asJson,
          "sidereal"      -> Json.Null, // one of these will be replaced
          "nonsidereal"   -> Json.Null, // one of these will be replaced
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

  case class ObservationInfo(
    id:                 Observation.Id,
    constraints:        ConstraintSet,
    posAngleConstraint: PosAngleConstraint,
    optWavelength:      Option[Wavelength],
    explicitBase:       Option[Coordinates]
  ) {
    def wavelength: Either[Error, Wavelength] =
      optWavelength.toRight(Error.GeneralError(s"No wavelength defined for observation $id."))

    private val AllAngles =
      NonEmptyList.fromListUnsafe(
        (0 until 360 by 10).map(a => Angle.fromDoubleDegrees(a.toDouble)).toList
      )

    val availabilityAngles: NonEmptyList[Angle] =
      posAngleConstraint match 
        case PosAngleConstraint.Fixed(a)               => NonEmptyList.of(a)
        case PosAngleConstraint.AllowFlip(a)           => NonEmptyList.of(a, a.flip)
        case PosAngleConstraint.ParallacticOverride(a) => NonEmptyList.of(a)
        case PosAngleConstraint.AverageParallactic     => AllAngles
        case PosAngleConstraint.Unbounded              => AllAngles

    def hash(generatorHash: Md5Hash): Md5Hash = {
      val md5 = MessageDigest.getInstance("MD5")

      md5.update(generatorHash.toByteArray)

      given HashBytes[ConstraintSet] = HashBytes.forJsonEncoder
      md5.update(constraints.hashBytes)

      // For our purposes, we don't care about the actual PosAngleConstraint, just what
      // angles we need to check.
      given HashBytes[NonEmptyList[Angle]] = HashBytes.forJsonEncoder
      md5.update(availabilityAngles.hashBytes)

      given HashBytes[Option[Wavelength]] = HashBytes.forJsonEncoder
      md5.update(optWavelength.hashBytes)

      given Encoder[Coordinates] = deriveEncoder
      md5.update(HashBytes.forJsonEncoder[Option[Coordinates]].hashBytes(explicitBase))

      Md5Hash.unsafeFromByteArray(md5.digest())
    }
  }

  private case class GeneratorInfo(
    digest: ExecutionDigest,
    params: GeneratorParams,
    hash:   Md5Hash
  ) {
    val timeEstimate                         = digest.fullTimeEstimate.sum
    val offsets                              = NonEmptyList.fromFoldable(digest.science.offsets.union(digest.acquisition.offsets))
    val (site, agsParams): (Site, AgsParams) = params.observingMode match
      case mode: gmos.longslit.Config.GmosNorth =>
        (Site.GN, AgsParams.GmosAgsParams(mode.fpu.asLeft.some, PortDisposition.Side))
      case mode: gmos.longslit.Config.GmosSouth =>
        (Site.GS, AgsParams.GmosAgsParams(mode.fpu.asRight.some, PortDisposition.Side))

  }

  def instantiate[F[_]: Concurrent](
    httpClient:             Client[F],
    itcClient:              ItcClient[F],
    commitHash:             CommitHash,
    timeEstimateCalculator: TimeEstimateCalculator.ForInstrumentMode
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

      def getAvailabilityHash(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F]
      ): F[Option[Md5Hash]] = {
        val af = Statements.getGuideAvailabilityHash(user, pid, oid)
        session
          .prepareR(af.fragment.query(md5_hash))
          .use(_.option(af.argument))
      }

      def insertOrUpdateAvailabilityHash(pid: Program.Id, oid: Observation.Id, hash: Md5Hash)(using
        Transaction[F]
      ): F[Unit] = {
        val af = Statements.insertOrUpdateGuideAvailabilityHash(user, pid, oid, hash)
        session
          .prepareR(af.fragment.command)
          .use(_.execute(af.argument).void)
      }

      def getAvailabilityPeriods(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F]
      ): F[List[AvailabilityPeriod]] = {
        val af = Statements.getAvailabilityPeriods(user, pid, oid)
        session
          .prepareR(af.fragment.query(Decoders.availability_period))
          .use(_.stream(af.argument, chunkSize = 1024).compile.toList)
      }

      def insertAvailabilityPeriods(pid: Program.Id, oid: Observation.Id, aps: List[AvailabilityPeriod])(using
        Transaction[F]
      ): F[Unit] = {
        val af = Statements.insertManyAvailabilityPeriods(user, pid, oid, aps)
        session
          .prepareR(af.fragment.command)
          .use(_.execute(af.argument).void)
      }

      def deleteAvailabilityPeriods(pid: Program.Id, oid: Observation.Id)(using Transaction[F]): F[Unit] = {
        val af = Statements.deleteAvailabilityPeriods(user, pid, oid)
        session
          .prepareR(af.fragment.command)
          .use(_.execute(af.argument).void)
      }

      def getFromCacheOrEmpty(pid: Program.Id, oid: Observation.Id, newHash: Md5Hash)(
        using NoTransaction[F]
      ): F[ContiguousTimestampMap[List[Angle]]] =
        getAvailabilityHash(pid, oid).flatMap(oldHash =>
          if (oldHash.exists(_ === newHash))
            getAvailabilityPeriods(pid, oid)
              .map(l => 
                // If for some reason the cache is invalid, we'll just ignore it
                ContiguousTimestampMap.fromList(l.map(ap => (ap.period, ap.posAngles)))
                  .getOrElse(ContiguousTimestampMap.empty[List[Angle]])
              )
          else ContiguousTimestampMap.empty[List[Angle]].pure[F]
        )
      
      def cacheAvailability(
        pid:          Program.Id,
        oid:          Observation.Id,
        hash:         Md5Hash,
        availability: ContiguousTimestampMap[List[Angle]]
      ): F[Unit] =
        services.transactionally {
          deleteAvailabilityPeriods(pid, oid) >>
          insertOrUpdateAvailabilityHash(pid, oid, hash) >>
          insertAvailabilityPeriods(pid, oid, availability.intervals.toList.map(AvailabilityPeriod.fromTuple))
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
        generator(commitHash, itcClient, timeEstimateCalculator)
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

      def buildAvailabilityAndCache(
        pid:             Program.Id,
        requestedPeriod: TimestampInterval,
        neededPeriods:   NonEmptyList[TimestampInterval],
        obsInfo:         ObservationInfo,
        genInfo:         GeneratorInfo,
        currentAvail:    ContiguousTimestampMap[List[Angle]],
        newHash:         Md5Hash
      ): F[Either[Error, ContiguousTimestampMap[List[Angle]]]] = 
        (for {
          wavelength   <- EitherT.fromEither(obsInfo.wavelength)
          asterism     <- EitherT(getAsterism(pid, obsInfo.id))
          tracking      = ObjectTracking.fromAsterism(asterism)
          candPeriod    = neededPeriods.tail.fold(neededPeriods.head)((a, b) => a.span(b))
          candidates   <- EitherT(
                           getCandidates(obsInfo.id, candPeriod.start, candPeriod.end, tracking, wavelength, obsInfo.constraints)
                          )
          positions     = getPositions(obsInfo.availabilityAngles, genInfo.offsets)
          neededLists  <- EitherT.fromEither(
                            neededPeriods.traverse(p => 
                              buildAvailabilityList(p, obsInfo, genInfo, wavelength, asterism, tracking, candidates, positions)
                            )
                          )
          availability <- EitherT.fromEither(
                            neededLists.foldLeft(currentAvail.some)((acc, ele) => acc.flatMap(_.union(ele)))
                              .toRight(Error.GeneralError("Error creating guide availability"))
                          )
          _            <- EitherT.right(cacheAvailability(pid, obsInfo.id, newHash, availability))
        } yield availability).value

      def buildAvailabilityList(
        period:     TimestampInterval,
        obsInfo:    ObservationInfo,
        genInfo:    GeneratorInfo,
        wavelength: Wavelength,
        asterism:   NonEmptyList[Target],
        tracking:   ObjectTracking,
        candidates: List[GuideStarCandidate],
        positions:  NonEmptyList[AgsPosition]
      ): Either[Error, ContiguousTimestampMap[List[Angle]]] = {
        @scala.annotation.tailrec
        def go(startTime: Timestamp, accum: ContiguousTimestampMap[List[Angle]]): Either[Error, ContiguousTimestampMap[List[Angle]]] = {
          val eap =
            buildAvailabilityPeriod(startTime, period.end, obsInfo, genInfo, wavelength, asterism, tracking, candidates, positions)
          eap match
            case Left(error)                      => error.asLeft
            case Right(ap) if ap.period.end < period.end => go(ap.period.end, accum.unsafeAdd(ap.period, ap.posAngles))
            case Right(ap)                        => 
              val newPeriod = TimestampInterval.between(ap.period.start, period.end)
              accum.unsafeAdd(newPeriod, ap.posAngles).asRight
        }
        candidates match
          case Nil => ContiguousTimestampMap.single(period, List.empty).asRight
          case _   => go(period.start, ContiguousTimestampMap.empty[List[Angle]])
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
                               obsInfo.availabilityAngles,
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
                               .plusMicrosOption(genInfo.timeEstimate.toMicroseconds)
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
                              .anglesToTestAt(genInfo.site, baseTracking, obsTime.toInstant, genInfo.timeEstimate.toDuration)
                              .toRight(Error.GeneralError(s"No angles to test for guide target candidates for observation $oid."))
                           )
          positions      = getPositions(angles, genInfo.offsets)
          usable         = processCandidates(obsInfo, wavelength, genInfo, baseCoords, scienceCoords, positions, candidates)
        } yield usable.toGuideEnvironments.toList).value

      override def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(using
        NoTransaction[F]
      ): F[Either[Error, List[AvailabilityPeriod]]] =
        (for {
          _             <- EitherT.fromEither(
                             if (period.boundedTimeSpan <= maxAvailabilityPeriod) ().asRight
                             else Error.GeneralError(
                              s"Period for guide availability cannot be greater than $maxAvailabilityPeriodDays days."
                             ).asLeft
                           )
          obsInfo       <- EitherT(getObservationInfo(pid, oid))
          genInfo       <- EitherT(getGeneratorInfo(pid, oid))
          newHash        = obsInfo.hash(genInfo.hash)
          currentAvail  <- EitherT.right(getFromCacheOrEmpty(pid, oid, newHash))
          missingPeriods = currentAvail.findMissingIntervals(period)
          // only happens if we have disjoint periods too far apart. If so, we'll just replace the existing
          (neededPeriods, startAvail) = if (missingPeriods.exists(_.boundedTimeSpan > maxAvailabilityPeriod)) 
                             (NonEmptyList.of(period).some, ContiguousTimestampMap.empty[List[Angle]])
                           else (NonEmptyList.fromList(missingPeriods), currentAvail)
          // if we don't need anything, then we already have what we need
          fullAvail     <- neededPeriods.fold(EitherT.pure(startAvail))(nel =>
                             EitherT(buildAvailabilityAndCache(pid, period, nel, obsInfo, genInfo, startAvail, newHash))
                           )
          availability   = fullAvail.slice(period).intervals.toList.map(AvailabilityPeriod.fromTuple)
        } yield availability).value
    }

  object Statements {
    import ProgramService.Statements.andWhereUserAccess
    import ProgramService.Statements.whereUserAccess

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
        where obs.c_program_id     = $program_id
          and obs.c_observation_id = $observation_id
      """.apply(pid, oid) |+| andWhereUserAccess(user, pid)

    def getGuideAvailabilityHash(user: User, pid: Program.Id, oid: Observation.Id): AppliedFragment = 
      sql"""
        select c_hash
        from t_guide_availability
        where c_program_id     = $program_id
          and c_observation_id = $observation_id
      """.apply(pid, oid) |+| andWhereUserAccess(user, pid)

    def insertOrUpdateGuideAvailabilityHash(
      user: User,
      pid: Program.Id,
      oid: Observation.Id,
      hash: Md5Hash
    ): AppliedFragment = 
      sql"""
        insert into t_guide_availability (
          c_program_id,
          c_observation_id,
          c_hash
        )
        select
          $program_id,
          $observation_id,
          $md5_hash
      """.apply(pid, oid, hash) |+| 
      whereUserAccess(user, pid) |+|
      sql"""
        on conflict on constraint t_guide_availability_pkey do update
          set c_hash = $md5_hash
      """.apply(hash)
    
    def getAvailabilityPeriods(user: User, pid: Program.Id, oid: Observation.Id): AppliedFragment =
      sql"""
        select
          c_start,
          c_end,
          c_angles
        from t_guide_availability_period
        where c_program_id     = $program_id
          and c_observation_id = $observation_id
      """.apply(pid, oid) |+| andWhereUserAccess(user, pid)
    
    def insertManyAvailabilityPeriods(
      user: User,
      pid: Program.Id,
      oid: Observation.Id,
      periods: List[AvailabilityPeriod]): AppliedFragment = 
      sql"""
        insert into t_guide_availability_period (
          c_program_id,
          c_observation_id,
          c_start,
          c_end,
          c_angles
        ) values ${(
          program_id *:
          observation_id *:
          core_timestamp *:
          core_timestamp *:
          Decoders.angle_µas_list
        ).values.list(periods.length)}
      """
      .apply(
        periods.map { ap =>
          (pid, oid, ap.period.start, ap.period.end, ap.posAngles)
        }
      )

    def deleteAvailabilityPeriods(user: User, pid: Program.Id, oid: Observation.Id): AppliedFragment =
      sql"""
        delete from t_guide_availability_period
        where c_program_id     = $program_id
          and c_observation_id = $observation_id
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

    val angle_µas_list: Codec[List[Angle]] =
      _angle_µas.imap(_.toList)(l => Arr.fromFoldable(l))

    val availability_period: Codec[AvailabilityPeriod] =
      (timestamp_interval *: angle_µas_list).imap((period, angles) =>
        AvailabilityPeriod(period, angles)
      )(ap => (ap.period, ap.posAngles))
  }
}
