// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.Order.*
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
import fs2.text.utf8
import grackle.Result
import grackle.ResultT
import grackle.syntax.*
import io.circe.Encoder
import io.circe.Json
import io.circe.generic.semiauto.*
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.ags
import lucuma.ags.*
import lucuma.ags.AgsPosition
import lucuma.ags.GuideStarCandidate
import lucuma.ags.GuideStarName
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
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.itc.client.ItcClient
import lucuma.itc.client.json.given
import lucuma.odb.data.ContiguousTimestampMap
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.SetGuideTargetNameInput
import lucuma.odb.json.all.query.given
import lucuma.odb.json.target
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.util.Codecs.*
import natchez.Trace
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
  import GuideService.GuideEnvironment

  def getObjectTracking(pid: Program.Id, oid: Observation.Id)(using
    NoTransaction[F]
  ): F[Result[ObjectTracking]]

  def getGuideEnvironments(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(using
    NoTransaction[F]
  ): F[Result[List[GuideEnvironment]]]

  def getGuideEnvironment(pid: Program.Id, oid: Observation.Id)(using
    NoTransaction[F]
  ): F[Result[GuideEnvironment]]

  def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(using
    NoTransaction[F]
  ): F[Result[List[AvailabilityPeriod]]]
  
  def setGuideTargetName(input: SetGuideTargetNameInput)(
    using NoTransaction[F]): F[Result[Observation.Id]]

  def getGuideTargetName(pid: Program.Id, oid: Observation.Id)(using 
    NoTransaction[F]
  ): F[Result[Option[NonEmptyString]]]
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

  private def generalError(error: String): OdbError =
    OdbError.GuideEnvironmentError(error.some)
  private def generatorError(error: Generator.Error): OdbError =
    OdbError.GuideEnvironmentError(error.format.some)
  private def gaiaError(error:String): OdbError =
    OdbError.GuideEnvironmentError(s"Error calling Gaia: $error".some)
  private def guideStarNameError(name: String): OdbError =
    OdbError.InvalidArgument(s"Invalid guide target name '$name'".some)


  case class ObservationInfo(
    id:                 Observation.Id,
    programId:          Program.Id,
    constraints:        ConstraintSet,
    posAngleConstraint: PosAngleConstraint,
    optWavelength:      Option[Wavelength],
    explicitBase:       Option[Coordinates],
    optObsTime:         Option[Timestamp],
    optObsDuration:     Option[TimeSpan],
    guideStarName:      Option[GuideStarName],
    guideStarHash:      Option[Md5Hash]
  ) {
    def wavelength: Result[Wavelength] =
      optWavelength.toResult(generalError(s"No wavelength defined for observation $id.").asProblem)

    def obsTime: Result[Timestamp] =
      optObsTime.toResult(generalError(s"Observation time not set for observation $id.").asProblem)

    def validGuideStarName(generatorHash:Md5Hash, remainingTime: TimeSpan): Option[GuideStarName] =
      (guideStarName, guideStarHash).flatMapN { (name, hash) =>
        val newHash = newGuideStarHash(generatorHash, remainingTime)
        if (hash === newHash) guideStarName
        else none
       }
    
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

    def availabilityHash(generatorHash: Md5Hash): Md5Hash = {
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

    def newGuideStarHash(generatorHash:Md5Hash, duration: TimeSpan): Md5Hash = {
      val md5 = MessageDigest.getInstance("MD5")

      md5.update(generatorHash.toByteArray)

      given HashBytes[ConstraintSet] = HashBytes.forJsonEncoder
      md5.update(constraints.hashBytes)

      given Encoder[PosAngleConstraint] = deriveEncoder
      given HashBytes[PosAngleConstraint] = HashBytes.forJsonEncoder
      md5.update(posAngleConstraint.hashBytes)
    
      given HashBytes[Option[Wavelength]] = HashBytes.forJsonEncoder
      md5.update(optWavelength.hashBytes)

      given Encoder[Coordinates] = deriveEncoder
      md5.update(HashBytes.forJsonEncoder[Option[Coordinates]].hashBytes(explicitBase))

      // changing time or duration doesn't necessarily invalidate the guide star, but 
      // we're not tracking what the "original" values are, so we can't say for sure...
      md5.update(optObsTime.hashBytes)
      md5.update(duration.hashBytes)

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

  def instantiate[F[_]: Concurrent: Trace](
    httpClient:             Client[F],
    itcClient:              ItcClient[F],
    commitHash:             CommitHash,
    timeEstimateCalculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  )(using Services[F]): GuideService[F] =
    new GuideService[F] {

      def getAsterism(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F]
      ): F[Result[NonEmptyList[Target]]] =
        asterismService
          .getAsterism(pid, oid)
          .map(l =>
            NonEmptyList
              .fromList(
                l.map(_._2)
              )
              .toResult(generalError(s"No targets have been defined for observation $oid.").asProblem)
          )

      def getObservationInfo(oid: Observation.Id)(using
        NoTransaction[F]
      ): F[Result[ObservationInfo]] = {
        val af = Statements.getObservationInfo(oid)
        session
          .prepareR(
            af.fragment.query(Decoders.obsInfoDecoder)
          )
          .use(
            _.option(af.argument).map(_.toResult(OdbError.InvalidObservation(oid).asProblem))
          )
      }

      def checkProgramAccess(pid: Program.Id, oid: Observation.Id): F[Result[Unit]] =
        services.transactionally(
          programService.userHasAccess(pid).map(hasAccess =>
            if (hasAccess) ().success
            else OdbError.InvalidObservation(oid).asFailure
          )
        )

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

      def updateGuideTargetName(
        pid: Program.Id,
        oid: Observation.Id,
        guideStarName: Option[GuideStarName],
        guideStarHash: Option[Md5Hash]
      ): F[Result[Observation.Id]] = 
        val af = Statements.updateGuideTargetName(user, pid, oid, guideStarName, guideStarHash)
        session
          .prepareR(af.fragment.query(observation_id))
          .use(_.option(af.argument))
          .map(_.fold(OdbError.InvalidObservation(oid).asFailure)(_.success))

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
      ): Result[ADQLQuery] =
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
          .toResult(
            generalError(s"Unable to get tracking information for asterism for observation $oid.").asProblem
          )

      def callGaia(
        oid:   Observation.Id,
        query: ADQLQuery
      ): F[Result[List[GuideStarCandidate]]] = {
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
          .map(_.success)
          // Should we have access to a logger in Services so we can log this instead of passing details on to the user?
          .handleError(e => gaiaError(e.getMessage()).asFailure)
      }

      def getAllCandidates(
        oid:         Observation.Id,
        start:       Timestamp,
        end:         Timestamp,
        tracking:    ObjectTracking,
        wavelength:  Wavelength,
        constraints: ConstraintSet
      ): F[Result[List[GuideStarCandidate]]] =
        (for {
          query      <- ResultT.fromResult(
                          getGaiaQuery(oid, start, end, tracking, probeArm.candidatesArea, wavelength, constraints)
                        )
          candidates <- ResultT(callGaia(oid, query))
        } yield candidates).value

      def getAllCandidatesNonEmpty(
        oid:         Observation.Id,
        start:       Timestamp,
        end:         Timestamp,
        tracking:    ObjectTracking,
        wavelength:  Wavelength,
        constraints: ConstraintSet
      ): F[Result[NonEmptyList[GuideStarCandidate]]] =
        (for {
          candidates <- ResultT(getAllCandidates(oid, start, end, tracking, wavelength, constraints))
          nel        <- ResultT.fromResult(
                          NonEmptyList.fromList(candidates)
                            .toResult(generalError("No potential guidestars found on Gaia.").asProblem)
                        )
        } yield nel).value

      def getGuideStarFromGaia(name: GuideStarName): F[Result[GuideStarCandidate]] =
        ResultT.fromResult(guideStarIdFromName(name)).flatMap { id => 
          given catalog: CatalogAdapter.Gaia = CatalogAdapter.Gaia3Lite
          val request = Request[F](Method.GET, CatalogSearch.gaiaSearchUriById(id),
                                  headers = Headers(("x-requested-with", "XMLHttpRequest"))
          ) 
          ResultT(
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
              .map(
                _.headOption
                .toResult(gaiaError(s"Star with id $id not found on Gaia.").asProblem)
              )
              // Should we have access to a logger in Services so we can log this instead of passing details on to the user?
              .handleError(e => gaiaError(e.getMessage()).asFailure)
          )
        }.value

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

      // TODO: Can go away after `guideEnvironments` is removed???
      extension (usables: List[AgsAnalysis.Usable])
        def toGuideEnvironments: List[GuideEnvironment] = usables.map(_.toGuideEnvironment)

      extension (usable: AgsAnalysis.Usable)
        def toGuideEnvironment: GuideEnvironment =
          val target = GuideStarCandidate.siderealTarget.reverseGet(usable.target)
          GuideEnvironment(usable.vignetting.head._1, List(GuideTarget(usable.guideProbe, target)))

      def getGeneratorInfo(
        pid: Program.Id,
        oid: Observation.Id
      ): F[Result[GeneratorInfo]] =
        generator(commitHash, itcClient, timeEstimateCalculator)
          .digestWithParamsAndHash(pid, oid)
          .map {
            case Right((d, p, h)) => GeneratorInfo(d, p, h).success
            case Left(ge)         => generatorError(ge).asFailure
          }

      def getPositions(
        angles:             NonEmptyList[Angle],
        offsets:            Option[NonEmptyList[Offset]],
      ): NonEmptyList[AgsPosition] =
        for {
          pa  <- angles
          off <- offsets.getOrElse(NonEmptyList.of(Offset.Zero))
        } yield AgsPosition(pa, off)

      // TODO: Can go away after `guideEnvironments` is removed???
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
      
      def chooseBestGuideStar(
        obsInfo:       ObservationInfo,
        wavelength:    Wavelength,
        genInfo:       GeneratorInfo,
        baseCoords:    Coordinates,
        scienceCoords: List[Coordinates],
        positions:     NonEmptyList[AgsPosition],
        candidates:    NonEmptyList[GuideStarCandidate]
      ): Option[AgsAnalysis.Usable] =
        Ags
          .agsAnalysis(obsInfo.constraints,
                       wavelength,
                       baseCoords,
                       scienceCoords,
                       positions,
                       genInfo.agsParams,
                       candidates.toList
          )
          .sortUsablePositions
          .collect { case usable: AgsAnalysis.Usable => usable }
          .headOption

      def buildAvailabilityAndCache(
        pid:             Program.Id,
        requestedPeriod: TimestampInterval,
        neededPeriods:   NonEmptyList[TimestampInterval],
        obsInfo:         ObservationInfo,
        genInfo:         GeneratorInfo,
        currentAvail:    ContiguousTimestampMap[List[Angle]],
        newHash:         Md5Hash
      ): F[Result[ContiguousTimestampMap[List[Angle]]]] = 
        (for {
          wavelength   <- ResultT.fromResult(obsInfo.wavelength)
          asterism     <- ResultT(getAsterism(pid, obsInfo.id))
          tracking      = ObjectTracking.fromAsterism(asterism)
          candPeriod    = neededPeriods.tail.fold(neededPeriods.head)((a, b) => a.span(b))
          candidates   <- ResultT(
                           getAllCandidates(obsInfo.id, candPeriod.start, candPeriod.end, tracking, wavelength, obsInfo.constraints)
                          )
          positions     = getPositions(obsInfo.availabilityAngles, genInfo.offsets)
          neededLists  <- ResultT.fromResult(
                            neededPeriods.traverse(p => 
                              buildAvailabilityList(p, obsInfo, genInfo, wavelength, asterism, tracking, candidates, positions)
                            )
                          )
          availability <- ResultT.fromResult(
                            neededLists.foldLeft(currentAvail.some)((acc, ele) => acc.flatMap(_.union(ele)))
                              .toResult(generalError("Error creating guide availability").asProblem)
                          )
          _            <- ResultT.liftF(cacheAvailability(pid, obsInfo.id, newHash, availability))
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
      ): Result[ContiguousTimestampMap[List[Angle]]] = {
        @scala.annotation.tailrec
        def go(startTime: Timestamp, accum: ContiguousTimestampMap[List[Angle]]): Either[OdbError, ContiguousTimestampMap[List[Angle]]] = {
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
          case Nil => ContiguousTimestampMap.single(period, List.empty).success
          case _   => go(period.start, ContiguousTimestampMap.empty[List[Angle]]).fold(_.asFailure, _.success)
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
      ): Either[OdbError, AvailabilityPeriod] =
        for {
          baseCoords      <- obsInfo.explicitBase
                               .orElse(
                                 tracking
                                   .at(start.toInstant)
                                   .map(_.value)
                               )
                               .toRight(
                                 generalError(s"Unable to get coordinates for asterism in observation ${obsInfo.id}")
                               )
          scienceCoords   <- asterism.toList
                               .traverse(t => ObjectTracking.fromTarget(t).at(start.toInstant).map(_.value))
                               .toRight(
                                 generalError(s"Unable to get coordinates for science targets in observation ${obsInfo.id}")
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

      def guideStarIdFromName(name: GuideStarName): Result[Long] = 
        name.toGaiaSourceId.toResult(generalError(s"Invalid guide star name `$name`").asProblem)

      override def getObjectTracking(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F]
      ): F[Result[ObjectTracking]] =
        (for {
          obsInfo       <- ResultT(getObservationInfo(oid))
          asterism      <- ResultT(getAsterism(pid, oid))
          baseTracking   = obsInfo.explicitBase.fold(ObjectTracking.fromAsterism(asterism))(ObjectTracking.constant)
        } yield baseTracking).value

      def lookupGuideStar(
        pid: Program.Id,
        oid: Observation.Id,
        oGuideStarName: Option[GuideStarName],
        obsInfo: ObservationInfo,
        genInfo: GeneratorInfo,
        obsTime: Timestamp,
        duration: TimeSpan
      ): F[Result[GuideEnvironment]] = 
        // If we got here, we either have the name but need to get all the details (they queried for more 
        // than name), or the name wasn't set or wasn't valid and we need to find all the candidates and 
        // select the best.
        (for {
          wavelength    <- ResultT.fromResult(obsInfo.wavelength)
          asterism      <- ResultT(getAsterism(pid, oid))
          baseTracking   = obsInfo.explicitBase.fold(ObjectTracking.fromAsterism(asterism))(ObjectTracking.constant)
          visitEnd      <- ResultT.fromResult(
                             obsTime
                               .plusMicrosOption(duration.toMicroseconds)
                               .toResult(generalError("Visit end time out of range").asProblem)
                           )
          candidates    <- ResultT(
                             oGuideStarName.fold(
                              getAllCandidatesNonEmpty(oid, obsTime, visitEnd, baseTracking, wavelength, obsInfo.constraints)
                             )(gsn => getGuideStarFromGaia(gsn).map(_.map(NonEmptyList.one)))
                           ).map(_.map(_.at(obsTime.toInstant)))
          baseCoords    <- ResultT.fromResult(
                             baseTracking.at(obsTime.toInstant).map(_.value)
                               .toResult(
                                 generalError(
                                   s"Unable to get coordinates for asterism in observation $oid"
                                 ).asProblem
                               )
                           )
          scienceCoords <- ResultT.fromResult(
                             asterism.toList
                               .traverse(t => ObjectTracking.fromTarget(t).at(obsTime.toInstant).map(_.value))
                               .toResult(
                                 generalError(
                                   s"Unable to get coordinates for science targets in observation $oid"
                                 ).asProblem
                               )
                           )
          angles        <- ResultT.fromResult(
                             obsInfo.posAngleConstraint
                              .anglesToTestAt(genInfo.site, baseTracking, obsTime.toInstant, genInfo.timeEstimate.toDuration)
                              .toResult(generalError(s"No angles to test for guide target candidates for observation $oid.").asProblem)
                           )
          positions      = getPositions(angles, genInfo.offsets)
          optUsable      = chooseBestGuideStar(obsInfo, wavelength, genInfo, baseCoords, scienceCoords, positions, candidates)
          env           <- ResultT.fromResult(
                             optUsable
                              .map(_.toGuideEnvironment)
                              .toResult (
                                generalError(
                                  oGuideStarName.fold("No usable guidestars are available.")(name =>
                                    s"Guidestar $name is not usable.")
                                ).asProblem
                             )
                           )
        } yield env).value
      
      override def getGuideEnvironment(pid: Program.Id, oid: Observation.Id)(
        using NoTransaction[F]
      ): F[Result[GuideEnvironment]] = 
        Trace[F].span("getGuideEnvironment"):
          (for {
            obsInfo       <- ResultT(getObservationInfo(oid))
            obsTime       <- ResultT.fromResult(obsInfo.obsTime)
            wavelength    <- ResultT.fromResult(obsInfo.wavelength)
            asterism      <- ResultT(getAsterism(pid, oid))
            genInfo       <- ResultT(getGeneratorInfo(pid, oid))
            duration       = obsInfo.optObsDuration.getOrElse(genInfo.timeEstimate)
            oGSName        = obsInfo.validGuideStarName(genInfo.hash, duration)
            result        <- ResultT(lookupGuideStar(pid, oid, oGSName, obsInfo, genInfo, obsTime, duration))
          } yield result).value

      override def getGuideTargetName(pid: Program.Id, oid: Observation.Id)(
        using NoTransaction[F]
      ): F[Result[Option[NonEmptyString]]] = 
        (for {
          obsInfo    <- ResultT(getObservationInfo(oid))
          genInfo    <- ResultT.liftF(getGeneratorInfo(pid, oid)).map(_.toOption)
          oGSName    <- ResultT.pure(
                          genInfo.flatMap{ gi => 
                            val duration = obsInfo.optObsDuration.getOrElse(gi.timeEstimate)
                            obsInfo.validGuideStarName(gi.hash, duration)
                          }
                        )
        } yield oGSName.map(_.toNonEmptyString)).value

      // TODO: This can go away when Navigate is ready.
      override def getGuideEnvironments(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(
        using NoTransaction[F]
      ): F[Result[List[GuideEnvironment]]] =
        (for {
          obsInfo       <- ResultT(getObservationInfo(oid))
          wavelength    <- ResultT.fromResult(obsInfo.wavelength)
          asterism      <- ResultT(getAsterism(pid, oid))
          genInfo       <- ResultT(getGeneratorInfo(pid, oid))
          baseTracking   = obsInfo.explicitBase.fold(ObjectTracking.fromAsterism(asterism))(ObjectTracking.constant)
          visitEnd      <- ResultT.fromResult(
                             obsTime
                               .plusMicrosOption(genInfo.timeEstimate.toMicroseconds)
                               .toResult(generalError("Visit end time out of range").asProblem)
                           )
          candidates    <- ResultT(
                            getAllCandidates(oid, obsTime, visitEnd, baseTracking, wavelength, obsInfo.constraints)
                          ).map(_.map(_.at(obsTime.toInstant)))
          baseCoords    <- ResultT.fromResult(
                             baseTracking.at(obsTime.toInstant).map(_.value)
                               .toResult(
                                 generalError(s"Unable to get coordinates for asterism in observation $oid").asProblem
                               )
                           )
          scienceCoords <- ResultT.fromResult(
                             asterism.toList
                               .traverse(t => ObjectTracking.fromTarget(t).at(obsTime.toInstant).map(_.value))
                               .toResult(
                                 generalError(s"Unable to get coordinates for science targets in observation $oid").asProblem
                               )
                           )
          angles        <- ResultT.fromResult(
                             obsInfo.posAngleConstraint
                              .anglesToTestAt(genInfo.site, baseTracking, obsTime.toInstant, genInfo.timeEstimate.toDuration)
                              .toResult(generalError(s"No angles to test for guide target candidates for observation $oid.").asProblem)
                           )
          positions      = getPositions(angles, genInfo.offsets)
          usable         = processCandidates(obsInfo, wavelength, genInfo, baseCoords, scienceCoords, positions, candidates)
        } yield usable.toGuideEnvironments.toList).value

      override def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(
        using NoTransaction[F]
      ): F[Result[List[AvailabilityPeriod]]] =
        Trace[F].span("getGuideAvailability"):
          (for {
            _             <- ResultT.fromResult(
                              if (period.boundedTimeSpan <= maxAvailabilityPeriod) ().success
                              else generalError(
                                s"Period for guide availability cannot be greater than $maxAvailabilityPeriodDays days."
                              ).asFailure
                            )
            obsInfo       <- ResultT(getObservationInfo(oid))
            genInfo       <- ResultT(getGeneratorInfo(pid, oid))
            newHash        = obsInfo.availabilityHash(genInfo.hash)
            currentAvail  <- ResultT.liftF(getFromCacheOrEmpty(pid, oid, newHash))
            missingPeriods = currentAvail.findMissingIntervals(period)
            // only happens if we have disjoint periods too far apart. If so, we'll just replace the existing
            (neededPeriods, startAvail) = if (missingPeriods.exists(_.boundedTimeSpan > maxAvailabilityPeriod)) 
                              (NonEmptyList.of(period).some, ContiguousTimestampMap.empty[List[Angle]])
                            else (NonEmptyList.fromList(missingPeriods), currentAvail)
            // if we don't need anything, then we already have what we need
            fullAvail     <- neededPeriods.fold(ResultT.pure(startAvail))(nel =>
                              ResultT(buildAvailabilityAndCache(pid, period, nel, obsInfo, genInfo, startAvail, newHash))
                            )
            availability   = fullAvail.slice(period).intervals.toList.map(AvailabilityPeriod.fromTuple)
          } yield availability).value

      def setGuideTargetNameImpl(obsInfo: ObservationInfo, targetName: Option[NonEmptyString]): F[Result[Observation.Id]] =
        targetName.fold(updateGuideTargetName(obsInfo.programId, obsInfo.id, none, none)){ name =>
          (for {
            gsn    <- ResultT.fromResult(
                        GuideStarName.from(name.value).toOption.toResult(guideStarNameError(name.value).asProblem)
                      )
            genInfo  <- ResultT(getGeneratorInfo(obsInfo.programId, obsInfo.id))
            duration  = obsInfo.optObsDuration.getOrElse(genInfo.timeEstimate)
            hash      = obsInfo.newGuideStarHash(genInfo.hash, duration)
            result   <- ResultT(updateGuideTargetName(obsInfo.programId, obsInfo.id, gsn.some, hash.some))
          } yield result).value
        }

      override def setGuideTargetName(input: SetGuideTargetNameInput)(
        using NoTransaction[F]): F[Result[Observation.Id]] = 
          Trace[F].span("setGuideTargetName"):
            (for {
            obsId   <- ResultT(observationService.resolveOid(input.observationId, input.observationRef))
            obsInfo <- ResultT(getObservationInfo(obsId))
            _       <- ResultT(checkProgramAccess(obsInfo.programId, obsInfo.id))
            result  <- ResultT(setGuideTargetNameImpl(obsInfo, input.targetName))
          } yield result).value
    }

  object Statements {
    import ProgramService.Statements.andWhereUserAccess
    import ProgramService.Statements.whereUserAccess

    def getObservationInfo(oid: Observation.Id): AppliedFragment =
      sql"""
        select
          obs.c_observation_id,
          obs.c_program_id,
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
          obs.c_explicit_dec,
          obs.c_observation_time,
          obs.c_observation_duration,
          obs.c_guide_target_name,
          obs.c_guide_target_hash
        from t_observation obs
        where obs.c_observation_id = $observation_id
      """.apply(oid)

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

    // both guideStarName and guideStarHash should either have values or be empty.
    def updateGuideTargetName(
      user: User,
      pid: Program.Id,
      oid: Observation.Id,
      guideStarName: Option[GuideStarName],
      guideStarHash: Option[Md5Hash]
    ): AppliedFragment =
      sql"""
        update t_observation
        set
          c_guide_target_name = ${guide_target_name.opt},
          c_guide_target_hash = ${md5_hash.opt}
        where c_program_id     = $program_id
          and c_observation_id = $observation_id
      """.apply(guideStarName, guideStarHash, pid, oid) |+| andWhereUserAccess(user, pid) |+|
      void"""  returning c_observation_id"""
  }

  private object Decoders {

    val obsInfoDecoder: Decoder[ObservationInfo] =
      (observation_id *:
        program_id *:
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
        declination.opt *:
        core_timestamp.opt *:
        time_span.opt *:
        guide_target_name.opt *:
        md5_hash.opt).emap {
        case (id, pid, cloud, image, sky, water, amMin, amMax, haMin, haMax, mode, angle, wavelength, ra, dec, time, duration, guidestarName, guidestarHash) =>
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
            ObservationInfo(id, pid, ConstraintSet(image, cloud, sky, water, elev), paConstraint, wavelength, explicitBase, time, duration, guidestarName, guidestarHash)
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
