// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order
import cats.Order.*
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Stream
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
import lucuma.ags.syntax.*
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.votable.*
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.GuideProbe
import lucuma.core.enums.GuideSpeed
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PortDisposition
import lucuma.core.enums.Site
import lucuma.core.enums.TrackType
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.gmos.candidatesArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.ProperMotion
import lucuma.core.math.Wavelength
import lucuma.core.model.AirMassBound
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.HourAngleBound
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.core.model.Tracking
import lucuma.core.model.User
import lucuma.core.model.probes
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.itc.client.ItcConstraintsInput
import lucuma.itc.client.ItcConstraintsInput.*
import lucuma.odb.data.ContiguousTimestampMap
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.graphql.input.SetGuideTargetNameInput
import lucuma.odb.graphql.mapping.AccessControl
import lucuma.odb.json.all.query.given
import lucuma.odb.json.target
import lucuma.odb.logic.Generator
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.flamingos2
import lucuma.odb.sequence.gmos
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.syntax.result.*
import lucuma.odb.util.Codecs.*
import monocle.Focus
import monocle.Lens
import natchez.Trace
import skunk.*
import skunk.implicits.*

import java.security.MessageDigest
import java.time.Duration
import java.time.Instant
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

import Services.Syntax.*

trait GuideService[F[_]] {
  import GuideService.AvailabilityPeriod
  import GuideService.GuideEnvironment

  def getGuideEnvironments(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(using
    NoTransaction[F], SuperUserAccess
  ): F[Result[List[GuideEnvironment]]]

  def getGuideEnvironment(pid: Program.Id, oid: Observation.Id)(using
    NoTransaction[F], SuperUserAccess
  ): F[Result[GuideEnvironment]]

  def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(using
    NoTransaction[F], SuperUserAccess
  ): F[Result[List[AvailabilityPeriod]]]

  def setGuideTargetName(
    checked: AccessControl.CheckedWithId[SetGuideTargetNameInput, Observation.Id]
  )(using NoTransaction[F]): F[Result[Observation.Id]]

  def getGuideTargetName(pid: Program.Id, oid: Observation.Id)(using
    NoTransaction[F], SuperUserAccess
  ): F[Result[Option[NonEmptyString]]]
}

object GuideService {
  // if any science target or guide star candidate moves more than this many milliarcseconds,
  // we consider it to potentially invalidate the availability.
  val invalidThreshold = Angle.milliarcseconds.reverseGet(100)

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

    val posAngle: Lens[GuideEnvironment, Angle] =
      Focus[GuideEnvironment](_.posAngle)

    val guideTargets: Lens[GuideEnvironment, List[GuideTarget]] =
      Focus[GuideEnvironment](_.guideTargets)
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
  private def generatorError(error: OdbError): OdbError =
    OdbError.GuideEnvironmentError(error.message.some)
  private def gaiaError(error:String): OdbError =
    OdbError.GuideEnvironmentError(s"Error calling Gaia: $error".some)
  private def guideStarNameError(name: String): OdbError =
    OdbError.InvalidArgument(s"Invalid guide target name '$name'".some)

  case class ObservationInfo(
    id:                  Observation.Id,
    programId:           Program.Id,
    constraints:         ConstraintSet,
    posAngleConstraint:  PosAngleConstraint,
    explicitBase:        Option[Coordinates],
    optObsTime:          Option[Timestamp],
    optObsDuration:      Option[TimeSpan],
    guideStarName:       Option[GuideStarName],
    guideStarHash:       Option[Md5Hash],
    blindOffsetTargetId: Option[Target.Id]
  ) {
    def obsTime: Result[Timestamp] =
      optObsTime.toResult(generalError(s"Observation time not set for observation $id.").asProblem)

    def obsDuration: Result[TimeSpan] =
      optObsDuration.toResult(generalError(s"Observation duration not set for observation $id.").asProblem)

    def validGuideStarName(generatorHash:Md5Hash): Option[GuideStarName] =
      guideStarHash.flatMap { hash =>
        val newHash = newGuideStarHash(generatorHash)
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

      given HashBytes[ItcConstraintsInput] = HashBytes.forJsonEncoder
      md5.update(constraints.toInput.hashBytes)

      // For our purposes, we don't care about the actual PosAngleConstraint, just what
      // angles we need to check.
      given HashBytes[NonEmptyList[Angle]] = HashBytes.forJsonEncoder
      md5.update(availabilityAngles.hashBytes)

      given Encoder[Coordinates] = deriveEncoder
      md5.update(HashBytes.forJsonEncoder[Option[Coordinates]].hashBytes(explicitBase))

      given Encoder[Target.Id] = deriveEncoder
      md5.update(HashBytes.forJsonEncoder[Option[Target.Id]].hashBytes(blindOffsetTargetId))

      Md5Hash.unsafeFromByteArray(md5.digest())
    }

    def newGuideStarHash(generatorHash:Md5Hash): Md5Hash = {
      val md5 = MessageDigest.getInstance("MD5")

      md5.update(generatorHash.toByteArray)

      given HashBytes[ItcConstraintsInput] = HashBytes.forJsonEncoder
      md5.update(constraints.toInput.hashBytes)

      given Encoder[PosAngleConstraint] = deriveEncoder
      given HashBytes[PosAngleConstraint] = HashBytes.forJsonEncoder
      md5.update(posAngleConstraint.hashBytes)

      given Encoder[Coordinates] = deriveEncoder
      md5.update(HashBytes.forJsonEncoder[Option[Coordinates]].hashBytes(explicitBase))

      // changing time or duration doesn't necessarily invalidate the guide star, but
      // we're not tracking what the "original" values are, so we can't say for sure...
      md5.update(optObsTime.hashBytes)
      md5.update(optObsDuration.hashBytes)

      Md5Hash.unsafeFromByteArray(md5.digest())
    }
  }

  private case class GeneratorInfo(
    digest: ExecutionDigest,
    params: GeneratorParams,
    hash:   Md5Hash
  ) {
    val timeEstimate = digest.fullTimeEstimate.sum
    val setupTime    = digest.setup.full
    val acqOffsets   = NonEmptySet.fromSet(digest.acquisition.telescopeConfigs).flatMap(_.asAcqOffsets)
    val sciOffsets   = NonEmptySet.fromSet(digest.science.telescopeConfigs).flatMap(_.asSciOffsets)

    val (site, observingModeType, agsWavelength): (Site, ObservingModeType, Wavelength) =
      params.observingMode match
        case mode: gmos.longslit.Config.GmosNorth =>
          (Site.GN, ObservingModeType.GmosNorthLongSlit, mode.centralWavelength)
        case mode: gmos.longslit.Config.GmosSouth =>
          (Site.GS, ObservingModeType.GmosSouthLongSlit, mode.centralWavelength)
        case gmos.imaging.Config.GmosNorth(filters = filters) =>
          (Site.GN, ObservingModeType.GmosNorthImaging, filters.map(_.filter.wavelength).maximum)
        case gmos.imaging.Config.GmosSouth(filters = filters) =>
          (Site.GS, ObservingModeType.GmosSouthImaging, filters.map(_.filter.wavelength).maximum)
        case mode: flamingos2.longslit.Config =>
          (Site.GS, ObservingModeType.Flamingos2LongSlit, mode.filter.wavelength)

    def agsParamsFor(trackType: TrackType): Option[AgsParams] =
      probes.guideProbe(observingModeType, trackType).flatMap: probe =>
        (params.observingMode, probe) match
          case (gmos.longslit.Config.GmosNorth(fpu = fpu), GuideProbe.GmosOIWFS) =>
            AgsParams.GmosLongSlit(fpu.asLeft, PortDisposition.Side).some
          case (gmos.longslit.Config.GmosNorth(fpu = fpu), GuideProbe.PWFS1) =>
            AgsParams.GmosLongSlit(fpu.asLeft, PortDisposition.Side).withPWFS1.some
          case (gmos.longslit.Config.GmosNorth(fpu = fpu), GuideProbe.PWFS2) =>
            AgsParams.GmosLongSlit(fpu.asLeft, PortDisposition.Side).withPWFS2.some
          case (gmos.longslit.Config.GmosSouth(fpu = fpu), GuideProbe.GmosOIWFS) =>
            AgsParams.GmosLongSlit(fpu.asRight, PortDisposition.Side).some
          case (gmos.longslit.Config.GmosSouth(fpu = fpu), GuideProbe.PWFS1) =>
            AgsParams.GmosLongSlit(fpu.asRight, PortDisposition.Side).withPWFS1.some
          case (gmos.longslit.Config.GmosSouth(fpu = fpu), GuideProbe.PWFS2) =>
            AgsParams.GmosLongSlit(fpu.asRight, PortDisposition.Side).withPWFS2.some
          case (flamingos2.longslit.Config(fpu = fpu), GuideProbe.Flamingos2OIWFS) =>
            AgsParams.Flamingos2LongSlit(Flamingos2LyotWheel.F16, Flamingos2FpuMask.Builtin(fpu), PortDisposition.Side).some
          case (flamingos2.longslit.Config(fpu = fpu), GuideProbe.PWFS1) =>
            AgsParams.Flamingos2LongSlit(Flamingos2LyotWheel.F16, Flamingos2FpuMask.Builtin(fpu), PortDisposition.Side).withPWFS1.some
          case (flamingos2.longslit.Config(fpu = fpu), GuideProbe.PWFS2) =>
            AgsParams.Flamingos2LongSlit(Flamingos2LyotWheel.F16, Flamingos2FpuMask.Builtin(fpu), PortDisposition.Side).withPWFS2.some
          case (_: gmos.imaging.Config.GmosNorth | _: gmos.imaging.Config.GmosSouth, GuideProbe.GmosOIWFS) =>
            AgsParams.GmosImaging(PortDisposition.Side).some
          case (_: gmos.imaging.Config.GmosNorth | _: gmos.imaging.Config.GmosSouth, GuideProbe.PWFS1) =>
            AgsParams.GmosImaging(PortDisposition.Side).withPWFS1.some
          case (_: gmos.imaging.Config.GmosNorth | _: gmos.imaging.Config.GmosSouth, GuideProbe.PWFS2) =>
            AgsParams.GmosImaging(PortDisposition.Side).withPWFS2.some
          case _ =>
            none

    def getScienceStartTime(obsTime: Timestamp): Timestamp = obsTime +| setupTime
    def getScienceDuration(obsDuration: TimeSpan, obsId: Observation.Id): Result[TimeSpan] =
      // Temporarily(?) disable check for too long of a duration for sc-5322
      // if (obsDuration > timeEstimate)
      //   generalError(s"Observation duration of ${obsDuration.format} exceeds the remaining time of ${timeEstimate.format} for observation $obsId.").asFailure
      // else
      obsDuration.subtract(setupTime)
          .filter(_.nonZero)
          .toResult(generalError(s"Observation duration of ${obsDuration.format} is less than the setup time of ${setupTime.format} for observation $obsId.").asProblem)
  }

  def instantiate[F[_]: Concurrent: Services: Trace](
    gaiaClient:             GaiaClient[F],
  ): GuideService[F] =
    new GuideService[F] {

      def getAsterism(pid: Program.Id, oid: Observation.Id)(using
        NoTransaction[F], SuperUserAccess
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
            _.option(af.argument).map(_.toResult(OdbError.InvalidObservation(oid, Some(s"Could not compute observation info for $oid.")).asProblem))
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

      @annotation.nowarn("msg=unused implicit parameter")
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

      @annotation.nowarn("msg=unused implicit parameter")
      def insertAvailabilityPeriods(pid: Program.Id, oid: Observation.Id, aps: List[AvailabilityPeriod])(using
        Transaction[F]
      ): F[Unit] = {
        val af = Statements.insertManyAvailabilityPeriods(pid, oid, aps)
        session
          .prepareR(af.fragment.command)
          .use(_.execute(af.argument).void)
      }

      @annotation.nowarn("msg=unused implicit parameter")
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
          .map(_.fold(OdbError.InvalidObservation(oid, Some(s"Failed to update guide target name for $oid.")).asFailure)(_.success))

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
        tracking:        Tracking,
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
              NonEmptyList.of(a, b),
              shapeConstraint,
              brightnessConstraints.some
            )
          }
          .toResult(
            generalError(s"Unable to get tracking information for asterism for observation $oid.").asProblem
          )

      def callGaia(
        query: ADQLQuery
      ): F[Result[List[(Target.Sidereal, GuideStarCandidate)]]] =
        Trace[F].span("callGaia"):
          val MaxTargets                     = 100
          given ADQLInterpreter          = ADQLInterpreter.nTarget(MaxTargets)
          gaiaClient
            .query(query)
            .map:
              _.collect { case Right(s) => (s.target, GuideStarCandidate.siderealTarget.get(s.target))}
                .success
              // Should we have access to a logger in Services so we can log this instead of passing details on to the user?
            .handleError(e => gaiaError(e.getMessage()).asFailure)

      def getAllCandidates(
        oid:         Observation.Id,
        start:       Timestamp,
        end:         Timestamp,
        tracking:    Tracking,
        wavelength:  Wavelength,
        constraints: ConstraintSet
      ): F[Result[List[(Target.Sidereal, GuideStarCandidate)]]] =
        (for {
          query      <- ResultT.fromResult(
                          getGaiaQuery(oid, start, end, tracking, candidatesArea.candidatesArea, wavelength, constraints)
                        )
          candidates <- ResultT(callGaia(query))
        } yield candidates).value

      def getAllCandidatesNonEmpty(
        oid:         Observation.Id,
        start:       Timestamp,
        end:         Timestamp,
        tracking:    Tracking,
        wavelength:  Wavelength,
        constraints: ConstraintSet
      ): F[Result[NonEmptyList[(Target.Sidereal, GuideStarCandidate)]]] =
        (for {
          candidates <- ResultT(getAllCandidates(oid, start, end, tracking, wavelength, constraints))
          nel        <- ResultT.fromResult(
                          NonEmptyList.fromList(candidates)
                            .toResult(generalError("No potential guidestars found on Gaia.").asProblem)
                        )
        } yield nel).value

      def getGuideStarFromGaia(name: GuideStarName): F[Result[(Target.Sidereal, GuideStarCandidate)]] =
        ResultT.fromResult(guideStarIdFromName(name)).flatMap { id =>
          ResultT(
            gaiaClient.queryById(id)
              .map:
                _.toOption
                  .map(s => (s.target, GuideStarCandidate.siderealTarget.get(s.target)))
                  .toResult(gaiaError(s"Star with id $id not found on Gaia.").asProblem)
              // Should we have access to a logger in Services so we can log this instead of passing details on to the user?
              .handleError(e => gaiaError(e.getMessage()).asFailure)
          )
        }.value


      extension (ts: Timestamp) def plusDaysTruncatedAndBounded(n: Int): Timestamp =
        Timestamp.fromInstantTruncatedAndBounded(ts.toInstant.plus(n, ChronoUnit.DAYS))

      /**
       * Find the first timestamp in `interval` for which the coordinates stray more than `invalidThreshold`
       * from the origin (i.e., coordinates at `interval.start`), or `Timestamp.Max` if the target never
       * exits the valid region. We only check once per day.
       */
      def findInvalidDate(tracking: Tracking, interval: TimestampInterval): Either[OdbError, Timestamp] = {

        // Get the coords and handle failure in the expected form
        def coordsAt(ts: Timestamp): Either[OdbError, Coordinates] =
          tracking.at(ts.toInstant)
            .toRight(generalError(s"Coordinates unavailable at ${interval.start.toInstant}."))

        // Get the origin, if we can
        coordsAt(interval.start).flatMap: origin =>

          def withinBoundsAt(ts: Timestamp): Either[OdbError, Boolean] =
            coordsAt(ts).map: coords =>
              origin.angularDistance(coords) > invalidThreshold

          // Start at `ts` and see how far the target has moved, looping back if we need to keep checking.
          @tailrec def go(ts: Timestamp): Either[OdbError, Timestamp] =
            if ts >= interval.end then
              // one last check at the endpoint
              withinBoundsAt(interval.end).map:
                case true  => Timestamp.Max // never exits
                case false => interval.end  // exits during the last day of the interval
            else
              // N.B. patten-match here to retain tailrec
              withinBoundsAt(ts) match
                case Left(err)    => Left(err)
                case Right(true)  => Right(ts)
                case Right(false) => go(ts.plusDaysTruncatedAndBounded(1))

          // Start at the beginning
          go(interval.start)

      }

      extension (sidereal: SiderealTracking)
        def masy: Double =
          sidereal.properMotion
            .map(pm => (pm.ra.masy.value.pow(2) + pm.dec.masy.value.pow(2)).toReal.sqrt.toDouble)
            .getOrElse(0.0)
        /** Optimized version of `findInvalidDate` for sidereal tracking, which always succeeds. */
        def invalidDate(startDate: Timestamp): Timestamp = {
          val speed = masy
          if (speed === 0.0) Timestamp.Max
          else {
            // This is approximate, but so is the invalidThreshold...
            // These should always be big values (until nonsidereal), but if it is 0, we'd go infinite loop.
            val daysTilInvalid = scala.math.max((Angle.milliarcseconds.get(invalidThreshold).toDouble / speed * 365).intValue, 1)
            startDate.plusDaysTruncatedAndBounded(daysTilInvalid)
          }
        }

      // TODO: Can go away after `guideEnvironments` is removed???
      extension (usables: List[AgsAnalysis.Usable])
        def toGuideEnvironments(originals: Map[Long, Target.Sidereal]): List[GuideEnvironment] =
          usables.map(_.toGuideEnvironment(originals)).flattenOption

      extension (usable: AgsAnalysis.Usable)
        def toGuideEnvironment(originals: Map[Long, Target.Sidereal]): Option[GuideEnvironment] =
          originals.get(usable.target.id).map: target =>
            GuideEnvironment(usable.posAngle, List(GuideTarget(usable.guideProbe, target)))

      def getGeneratorInfo(
        pid: Program.Id,
        oid: Observation.Id
      ): F[Result[GeneratorInfo]] =
        Services.asSuperUser:
          generator
            .digestWithParamsAndHash(pid, oid)
            .map:
              case Right((d, p, h)) => GeneratorInfo(d, p, h).success
              case Left(ge)         => generatorError(ge).asFailure

      // TODO: Can go away after `guideEnvironments` is removed???
      def processCandidates(
        obsInfo:       ObservationInfo,
        wavelength:    Wavelength,
        genInfo:       GeneratorInfo,
        baseCoords:    Coordinates,
        scienceCoords: List[Coordinates],
        blindOffset:   Option[Coordinates],
        angles:        NonEmptyList[Angle],
        candidates:    List[GuideStarCandidate],
        trackType:     TrackType
      ): List[AgsAnalysis.Usable] =
        genInfo.agsParamsFor(trackType).foldMap: params =>
          Ags
            .agsAnalysis(obsInfo.constraints,
                        wavelength,
                        baseCoords,
                        scienceCoords,
                        blindOffset,
                        angles,
                        genInfo.acqOffsets,
                        genInfo.sciOffsets,
                        params,
                        candidates
            )
            .sortUsablePositions

      def chooseBestGuideStar(
        obsInfo:       ObservationInfo,
        wavelength:    Wavelength,
        genInfo:       GeneratorInfo,
        baseCoords:    Coordinates,
        scienceCoords: List[Coordinates],
        blindOffset:   Option[Coordinates],
        angles:        NonEmptyList[Angle],
        candidates:    NonEmptyList[GuideStarCandidate],
        trackType:     TrackType
      ): Option[AgsAnalysis.Usable] =
        genInfo.agsParamsFor(trackType).flatMap: params =>
          Ags
            .agsAnalysis(obsInfo.constraints,
                        wavelength,
                        baseCoords,
                        scienceCoords,
                        blindOffset,
                        angles,
                        genInfo.acqOffsets,
                        genInfo.sciOffsets,
                        params,
                        candidates.toList
            )
            .sortUsablePositions
            .headOption

      def buildAvailabilityAndCache(
        pid:             Program.Id,
        neededPeriods:   NonEmptyList[TimestampInterval],
        obsInfo:         ObservationInfo,
        genInfo:         GeneratorInfo,
        currentAvail:    ContiguousTimestampMap[List[Angle]],
        blindOffset:     Option[Coordinates],
        newHash:         Md5Hash
      )(using SuperUserAccess): F[Result[ContiguousTimestampMap[List[Angle]]]] =
        val interval = TimestampInterval.between(neededPeriods.minimumBy(_.start).start, neededPeriods.maximumBy(_.end).end)
        (for {
          tracking     <- ResultT(trackingService.getTrackingSnapshot(obsInfo.id, interval, false).map(_.redeemFailure)) // treat failure as None
          candPeriod    = neededPeriods.tail.fold(neededPeriods.head)((a, b) => a.span(b))
          candidates   <- ResultT:
                           tracking match
                              case None    => Result.success(Nil).pure[F]
                              case Some(t) =>
                                getAllCandidates(
                                  obsInfo.id,
                                  candPeriod.start,
                                  candPeriod.end, t.base,
                                  genInfo.agsWavelength,
                                  obsInfo.constraints
                                )
          neededLists  <- ResultT.fromResult:
                            neededPeriods.traverse: p =>
                              tracking match
                                case None => Result.success(ContiguousTimestampMap.empty[List[Angle]])
                                case Some(t) =>
                                  buildAvailabilityList(
                                    p,
                                    obsInfo,
                                    genInfo,
                                    genInfo.agsWavelength,
                                    t.asterism.map(_._2),
                                    t.base,
                                    blindOffset,
                                    candidates.map(_._2)
                                  )
          availability <- ResultT.fromResult(
                            neededLists.foldLeft(currentAvail.some)((acc, ele) => acc.flatMap(_.union(ele)))
                              .toResult(generalError("Error creating guide availability").asProblem)
                          )
          _            <- ResultT.liftF(cacheAvailability(pid, obsInfo.id, newHash, availability))
        } yield availability).value

      def buildAvailabilityList(
        period:           TimestampInterval,
        obsInfo:          ObservationInfo,
        genInfo:          GeneratorInfo,
        wavelength:       Wavelength,
        asterismTracking: NonEmptyList[Tracking],
        baseTracking:     Tracking,
        blindOffset:      Option[Coordinates],
        candidates:       List[GuideStarCandidate]
      ): Result[ContiguousTimestampMap[List[Angle]]] = {
        @scala.annotation.tailrec
        def go(startTime: Timestamp, accum: ContiguousTimestampMap[List[Angle]]): Either[OdbError, ContiguousTimestampMap[List[Angle]]] = {
          val eap =
            buildAvailabilityPeriod(
              startTime,
              period.end,
              obsInfo,
              genInfo,
              wavelength,
              asterismTracking,
              baseTracking,
              blindOffset,
              candidates
            )
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
        start:            Timestamp,
        end:              Timestamp,
        obsInfo:          ObservationInfo,
        genInfo:          GeneratorInfo,
        wavelength:       Wavelength,
        asterismTracking: NonEmptyList[Tracking],
        baseTracking:     Tracking,
        blindOffset:      Option[Coordinates],
        candidates:       List[GuideStarCandidate]
      ): Either[OdbError, AvailabilityPeriod] =
        for {
          baseCoords      <- obsInfo.explicitBase
                               .orElse(baseTracking.at(start.toInstant))
                               .toRight(
                                 generalError(s"Unable to get coordinates for asterism in observation ${obsInfo.id}")
                               )
          scienceCoords   <- asterismTracking
                               .traverse(_.at(start.toInstant))
                               .toRight(
                                 generalError(s"Unable to get coordinates for all science targets in observation ${obsInfo.id}")
                               )

          scienceCutoff   <- asterismTracking
                              .traverse(findInvalidDate(_, TimestampInterval.between(start, end)))
                              .map(_.minimum)

          // we can stop testing candidates when all angles being tested have invalid dates that are farther
          // in the future than either the end time passed in, or a science candidate will be invalid
          endCutoff        = scienceCutoff.min(end)
          candidatesAt     = candidates.map(_.at(start.toInstant))
          trackType        = CompositeTracking(asterismTracking).trackType
          agsParams       <- genInfo.agsParamsFor(trackType).toRight(generalError(s"Unable to get AGS params for observation ${obsInfo.id}"))
          angleMap         = getAvailabilityMap(
                               candidatesAt,
                               start,
                               endCutoff,
                               obsInfo.constraints,
                               wavelength,
                               baseCoords,
                               scienceCoords.toList,
                               blindOffset,
                               obsInfo.availabilityAngles,
                               genInfo.acqOffsets,
                               genInfo.sciOffsets,
                               agsParams
                             )
          candidateCutoff  = angleMap.values.minOption.getOrElse(Timestamp.Max)
          finalEnd         = endCutoff.min(candidateCutoff)
        } yield AvailabilityPeriod(start, finalEnd, angleMap.keys.toList)

      def getAvailabilityMap(
        candidates:    List[GuideStarCandidate],
        start:         Timestamp,
        endCutoff:     Timestamp, // The oldest time we need to satisfy to allow short cutting
        constraints:   ConstraintSet,
        wavelength:    Wavelength,
        baseCoords:    Coordinates,
        scienceCoords: List[Coordinates],
        blindOffset:   Option[Coordinates],
        angles:        NonEmptyList[Angle],
        acqOffsets:    Option[AcquisitionOffsets],
        sciOffsets:    Option[ScienceOffsets],
        params:        AgsParams
      ): SortedMap[Angle, Timestamp] = {
        Stream.emits(candidates)
          .through(
            Ags.agsAnalysisStream(
              constraints,
              wavelength,
              baseCoords,
              scienceCoords,
              blindOffset,
              angles,
              acqOffsets,
              sciOffsets,
              params
            )
          )
          .collect { case u: AgsAnalysis.Usable => u }
          .scan(SortedMap.empty[Angle, Timestamp]){ (map, usable) =>
            val invalidDate = usable.target.tracking.invalidDate(start)
            val angle       = usable.posAngle
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

      def getBlindOffsetCoordinates(
        oid:     Observation.Id,
        instant: Instant
      ): F[Option[Coordinates]] =
          Trace[F].span("getBlindOffsetCoordinates"):
            val trackingStmt = Statements.getBlindOffsetTracking(oid)
            session.prepareR(
              trackingStmt.fragment.query(siderealTrackingDecoder)
            ).use(_.option(trackingStmt.argument)).map: tracking =>
              tracking.flatMap(_.at(instant))

      def lookupGuideStar(
        oid:                    Observation.Id,
        oGuideStarName:         Option[GuideStarName],
        obsInfo:                ObservationInfo,
        genInfo:                GeneratorInfo,
        obsTime:                Timestamp,
        obsDuration:            TimeSpan,
        scienceTime:            Timestamp,
        scienceDuration:        TimeSpan
      )(using SuperUserAccess): F[Result[GuideEnvironment]] =
        // If we got here, we either have the name but need to get all the details (they queried for more
        // than name), or the name wasn't set or wasn't valid and we need to find all the candidates and
        // select the best.
        (for {

          visitEnd      <- ResultT.fromResult(
                             obsTime
                               .plusMicrosOption(obsDuration.toMicroseconds)
                               .toResult(generalError("Visit end time out of range").asProblem)
                           )

          tracking      <- ResultT(trackingService.getTrackingSnapshot(oid, TimestampInterval.empty(obsTime), false))
          baseTracking     = tracking.base
          asterismTracking = tracking.asterism.map(_._2) // discard the target ids

          original      <- ResultT(
                             oGuideStarName.fold(
                              getAllCandidatesNonEmpty(oid, obsTime, visitEnd, baseTracking, genInfo.agsWavelength, obsInfo.constraints)
                             )(gsn => getGuideStarFromGaia(gsn).map(_.map(NonEmptyList.one)))
                           )
          candidates     = original.map(_._2.at(obsTime.toInstant)) // PM corrected
          baseCoords    <- ResultT.fromResult(
                             baseTracking.at(obsTime.toInstant)
                               .toResult(
                                 generalError(
                                   s"Unable to get coordinates for asterism in observation $oid"
                                 ).asProblem
                               )
                           )
          scienceCoords <- ResultT.fromResult:
                             asterismTracking.toList
                               .traverse(_.at(obsTime.toInstant))
                               .toResult(
                                 generalError(
                                   s"Unable to get coordinates for science targets in observation $oid"
                                 ).asProblem
                               )

          angles        <- ResultT.fromResult(
                             obsInfo.posAngleConstraint
                              .anglesToTestAt(genInfo.site, baseTracking, scienceTime.toInstant, scienceDuration.toDuration)
                              .toResult(generalError(s"No angles to test for guide target candidates for observation $oid.").asProblem)
                           )
          blindOffsetOpt <- ResultT.liftF(getBlindOffsetCoordinates(oid, obsTime.toInstant))
          trackType      = CompositeTracking(asterismTracking).trackType
          optUsable      = chooseBestGuideStar(obsInfo, genInfo.agsWavelength, genInfo, baseCoords, scienceCoords, blindOffsetOpt, angles, candidates, trackType)
          tgts           = original.map(x => (x._2.id, x._1)).toList.toMap
          env            <- ResultT.fromResult(
                             optUsable
                              .flatMap(_.toGuideEnvironment(tgts))
                              .toResult (
                                generalError(
                                  oGuideStarName.fold("No usable guidestars are available.")(name =>
                                    s"Guidestar $name is not usable.")
                                ).asProblem
                             )
                           )
        } yield env).value

      override def getGuideEnvironment(pid: Program.Id, oid: Observation.Id)(
        using NoTransaction[F], SuperUserAccess
      ): F[Result[GuideEnvironment]] =
        Trace[F].span("getGuideEnvironment"):
          (for {
            obsInfo         <- ResultT(getObservationInfo(oid))
            obsTime         <- ResultT.fromResult(obsInfo.obsTime)
            obsDuration     <- ResultT.fromResult(obsInfo.obsDuration)
            asterism        <- ResultT(getAsterism(pid, oid))
            genInfo         <- ResultT(getGeneratorInfo(pid, oid))
            scienceDuration <- ResultT.fromResult(genInfo.getScienceDuration(obsDuration, oid))
            scienceStart     = genInfo.getScienceStartTime(obsTime)
            oGSName          = obsInfo.validGuideStarName(genInfo.hash)
            result          <- ResultT(lookupGuideStar(oid, oGSName, obsInfo, genInfo, obsTime, obsDuration, scienceStart, scienceDuration))
          } yield result).value

      override def getGuideTargetName(pid: Program.Id, oid: Observation.Id)(
        using NoTransaction[F], SuperUserAccess
      ): F[Result[Option[NonEmptyString]]] =
        (for {
          obsInfo    <- ResultT(getObservationInfo(oid))
          genInfo    <- ResultT.liftF(getGeneratorInfo(pid, oid)).map(_.toOption)
          oGSName    <- ResultT.pure(
                          genInfo.flatMap{ gi =>
                            obsInfo.validGuideStarName(gi.hash)
                          }
                        )
        } yield oGSName.map(_.toNonEmptyString)).value

      // deprecated - use getGuideEnvironment istead
      override def getGuideEnvironments(pid: Program.Id, oid: Observation.Id, obsTime: Timestamp)(
        using NoTransaction[F], SuperUserAccess
      ): F[Result[List[GuideEnvironment]]] =
        (for {
          obsInfo       <- ResultT(getObservationInfo(oid))
          genInfo       <- ResultT(getGeneratorInfo(pid, oid))

          tracking      <- ResultT(trackingService.getTrackingSnapshot(oid, TimestampInterval.empty(obsTime), false))
          baseTracking     = tracking.base // use explicit base if defined
          asterismTracking = tracking.asterism.map(_._2) // discard the target ids

          visitEnd      <- ResultT.fromResult(
                             obsTime
                               .plusMicrosOption(genInfo.timeEstimate.toMicroseconds)
                               .toResult(generalError("Visit end time out of range").asProblem)
                           )
          original <- ResultT(getAllCandidates(oid, obsTime, visitEnd, baseTracking, genInfo.agsWavelength, obsInfo.constraints))
          originals      = original.map(c => c._2.id -> c._1).toMap
          candidates     = original.map(_._2.at(obsTime.toInstant))
          baseCoords    <- ResultT.fromResult(
                             baseTracking.at(obsTime.toInstant)
                               .toResult(
                                 generalError(s"Unable to get coordinates for asterism in observation $oid").asProblem
                               )
                           )
          scienceCoords <- ResultT.fromResult(
                            asterismTracking
                              .toList
                              .traverse(_.at(obsTime.toInstant))
                              .toResult(
                                generalError(s"Unable to get coordinates for science targets in observation $oid").asProblem
                              )
                           )
          angles        <- ResultT.fromResult(
                            obsInfo
                              .posAngleConstraint
                              .anglesToTestAt(genInfo.site, baseTracking, obsTime.toInstant, genInfo.timeEstimate.toDuration)
                              .toResult(generalError(s"No angles to test for guide target candidates for observation $oid.").asProblem)
                           )
          blindOffsetOpt <- ResultT.liftF(getBlindOffsetCoordinates(oid, obsTime.toInstant))
          trackType      = CompositeTracking(asterismTracking).trackType
          usable         = processCandidates(obsInfo, genInfo.agsWavelength, genInfo, baseCoords, scienceCoords, blindOffsetOpt, angles, candidates, trackType)
        } yield usable.toGuideEnvironments(originals).toList).value

      override def getGuideAvailability(pid: Program.Id, oid: Observation.Id, period: TimestampInterval)(
        using NoTransaction[F], SuperUserAccess
      ): F[Result[List[AvailabilityPeriod]]] =
        Trace[F].span("getGuideAvailability"):
          (for {
            _             <- ResultT.fromResult(
                              if (period.timeSpan <= maxAvailabilityPeriod) ().success
                              else generalError(
                                s"Period for guide availability cannot be greater than $maxAvailabilityPeriodDays days."
                              ).asFailure
                            )
            obsInfo       <- ResultT(getObservationInfo(oid))
            genInfo       <- ResultT(getGeneratorInfo(pid, oid))
            blindOffset   <- ResultT.liftF(getBlindOffsetCoordinates(oid, period.start.toInstant))
            newHash        = obsInfo.availabilityHash(genInfo.hash)
            currentAvail  <- ResultT.liftF(getFromCacheOrEmpty(pid, oid, newHash))
            missingPeriods = currentAvail.findMissingIntervals(period)
            // only happens if we have disjoint periods too far apart. If so, we'll just replace the existing
            (neededPeriods, startAvail) = if (missingPeriods.exists(_.timeSpan > maxAvailabilityPeriod))
                              (NonEmptyList.of(period).some, ContiguousTimestampMap.empty[List[Angle]])
                            else (NonEmptyList.fromList(missingPeriods), currentAvail)
            // if we don't need anything, then we already have what we need
            fullAvail     <- neededPeriods.fold(ResultT.pure(startAvail))(nel =>
                              ResultT(buildAvailabilityAndCache(pid, nel, obsInfo, genInfo, startAvail, blindOffset, newHash))
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
            hash      = obsInfo.newGuideStarHash(genInfo.hash)
            result   <- ResultT(updateGuideTargetName(obsInfo.programId, obsInfo.id, gsn.some, hash.some))
          } yield result).value
        }

      override def setGuideTargetName(checked: AccessControl.CheckedWithId[SetGuideTargetNameInput, Observation.Id])(
        using NoTransaction[F]): F[Result[Observation.Id]] =
          Trace[F].span("setGuideTargetName"):
            checked.foldWithId(
              OdbError.InvalidArgument().asFailureF
            ): (input, obsId) =>
              ResultT(getObservationInfo(obsId))
                .flatMap: obsInfo =>
                  ResultT(setGuideTargetNameImpl(obsInfo, input.targetName))
                .value

    }

  object Statements {
    import ProgramUserService.Statements.andWhereUserReadAccess
    import ProgramUserService.Statements.andWhereUserWriteAccess
    import ProgramUserService.Statements.whereUserWriteAccess

    def getObservationInfo(oid: Observation.Id): AppliedFragment =
      sql"""
        select
          c_observation_id,
          c_program_id,
          c_cloud_extinction,
          c_image_quality,
          c_sky_background,
          c_water_vapor,
          c_air_mass_min,
          c_air_mass_max,
          c_hour_angle_min,
          c_hour_angle_max,
          c_pac_mode,
          c_pac_angle,
          c_explicit_ra,
          c_explicit_dec,
          c_observation_time,
          c_observation_duration,
          c_guide_target_name,
          c_guide_target_hash,
          c_blind_offset_target_id
        from t_observation
        where c_observation_id = $observation_id
      """.apply(oid)

    def getBlindOffsetTracking(oid: Observation.Id): AppliedFragment =
      sql"""
        SELECT t.c_sid_ra, t.c_sid_dec, t.c_sid_epoch, t.c_sid_pm_ra, t.c_sid_pm_dec, t.c_sid_rv, t.c_sid_parallax
        FROM t_observation o
        INNER JOIN t_target t ON o.c_blind_offset_target_id = t.c_target_id
        WHERE o.c_observation_id = $observation_id
          AND t.c_existence = 'present'
          AND t.c_target_disposition = 'blind_offset'
      """.apply(oid)

    def getGuideAvailabilityHash(user: User, pid: Program.Id, oid: Observation.Id): AppliedFragment =
      sql"""
        select c_hash
        from t_guide_availability
        where c_program_id     = $program_id
          and c_observation_id = $observation_id
      """.apply(pid, oid) |+| andWhereUserReadAccess(user, pid)

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
      whereUserWriteAccess(user, pid) |+|
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
      """.apply(pid, oid) |+| andWhereUserReadAccess(user, pid)

    def insertManyAvailabilityPeriods(
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
          angle_µas_list
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
      """.apply(pid, oid) |+| andWhereUserWriteAccess(user, pid)

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
      """.apply(guideStarName, guideStarHash, pid, oid) |+| andWhereUserWriteAccess(user, pid) |+|
      void"""  returning c_observation_id"""
  }

  private object Decoders {

    val obsInfoDecoder: Decoder[ObservationInfo] =
      (observation_id              *:
        program_id                 *:
        cloud_extinction_preset    *:
        image_quality_preset       *:
        sky_background             *:
        water_vapor                *:
        air_mass_range_value.opt   *:
        air_mass_range_value.opt   *:
        hour_angle_range_value.opt *:
        hour_angle_range_value.opt *:
        pac_mode                   *:
        angle_µas                  *:
        right_ascension.opt        *:
        declination.opt            *:
        core_timestamp.opt         *:
        time_span.opt              *:
        guide_target_name.opt      *:
        md5_hash.opt               *:
        target_id.opt).emap {
        case (id, pid, cloud, image, sky, water, amMin, amMax, haMin, haMax, mode, angle, ra, dec, time, duration, guidestarName, guidestarHash, blindOffsetTargetId) =>
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
              aMin <- AirMassBound.fromBigDecimal(min.value).toOption
              aMax <- AirMassBound.fromBigDecimal(max.value).toOption
              am   <- ElevationRange.ByAirMass.FromOrderedBounds.getOption((aMin, aMax))
            } yield am)
              .orElse(
                for {
                  min  <- haMin
                  max  <- haMax
                  hMin <- HourAngleBound.from(min).toOption
                  hMax <- HourAngleBound.from(max).toOption
                  ha   <- ElevationRange.ByHourAngle.FromOrderedBounds.getOption((hMin, hMax))
                } yield ha
              )
              .toRight(s"Invalid elevation range in observation $id.")

          val explicitBase: Option[Coordinates] =
            (ra, dec).mapN(Coordinates(_, _))

          elevRange.map(elev =>
            ObservationInfo(id, pid, ConstraintSet(image, cloud, sky, water, elev), paConstraint, explicitBase, time, duration, guidestarName, guidestarHash, blindOffsetTargetId)
          )
      }

    val availability_period: Codec[AvailabilityPeriod] =
      (timestamp_interval *: angle_µas_list).imap((period, angles) =>
        AvailabilityPeriod(period, angles)
      )(ap => (ap.period, ap.posAngles))

  }
}
