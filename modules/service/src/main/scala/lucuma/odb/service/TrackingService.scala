// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Eval
import cats.Monad
import cats.Traverse
import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.all.*
import grackle.Result
import grackle.ResultT
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.EphemerisKey
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Opportunity
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.Tracking
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.horizons.HorizonsClient
import lucuma.horizons.HorizonsEphemeris
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.service.Services.Syntax.asterismService
import lucuma.odb.service.Services.Syntax.session
import lucuma.odb.service.Services.asSuperUser
import lucuma.odb.util.Codecs.*
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import skunk.Query
import skunk.syntax.all.*

import scala.annotation.nowarn
import scala.concurrent.duration.*

trait TrackingService[F[_]]:
  import TrackingService.Snapshot

  /**
   * Yield a mapping from each value in `keys` to a result containig either a tracking snapshot
   * or a pair containing the region and explicit base (if any) for observations containing opportunity
   * targets; or a failure if tracking information is not available. Any returned snapshot is 
   * guaranteed to be defined at all points on `interval`.
   * 
   * This is the most general method; all other methods on this interface are defined in terms of
   * this operation.
   */
  def getTrackingSnapshotOrRegion(
    keys: List[Observation.Id],
    interval: TimestampInterval
  ): F[Map[Observation.Id, Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]]]

  /**
   * Convenience method that calls `getTrackingSnapshotOrRegion` over an interval around `t` and
   * evaluates any computed tracking functions at that point.
   */
  def getCoordinatesSnapshotOrRegion(
    keys: List[Observation.Id],
    t: Timestamp
  ): F[Map[Observation.Id, Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]]]

  /** Single-target version of `getCoordinatesSnapshotOrRegion`. */
  def getTrackingSnapshotOrRegion(
    oid: Observation.Id, 
    interval: TimestampInterval
  ): F[Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]] 

  /** 
   * Convenience method that calls `getTrackingSnapshotOrRegion` that discards any resulting
   * region and turns it into a failure.
   */
  def getTrackingSnapshot(
    oid: Observation.Id, 
    interval: TimestampInterval
  ): F[Result[Snapshot[Tracking]]] 

  /** Single-target version of `getCoordinatesSnapshotOrRegion`. */
  def getCoordinatesSnapshotOrRegion(
    oid: Observation.Id, 
    t: Timestamp
  ): F[Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]] 

  /** 
   * Convenience method that calls `getCoordinatesSnapshotOrRegion` that discards any resulting
   * region and turns it into a failure.
   */
  def getCoordinatesSnapshot(
    oid: Observation.Id, 
    t: Timestamp
  ): F[Result[Snapshot[Coordinates]]] 




object TrackingService:


  extension (interval: TimestampInterval) 
    private def days: Int = interval.duration.toDays.toInt max 1 // always at least one day
    private def cadence: HorizonsClient.ElementsPerDay =
      import interval.duration
      if      duration <=  1.day then 24
      else if duration <=  2.day then 12
      else if duration <=  3.day then 8
      else if duration <=  4.day then 6
      else if duration <=  6.day then 4
      else if duration <= 12.day then 2
      else 1

  extension [F[_]: Monad, A](rt: ResultT[F, A]) def flatTap(f: A => ResultT[F, Unit]): ResultT[F, A] =
    rt.flatMap(a => f(a).as(a))

  /** A snapshot of information (typically `Tracking` or `Coordinates`) for an observation's base position and asterism. */
  case class Snapshot[+A](oid: Observation.Id, base: A, asterism: NonEmptyList[(Target.Id, A)]):

    /** If this is a tracking snapshot, evaluate it at a given time. */
    def at(t: Timestamp)(using A <:< Tracking): Result[Snapshot[Coordinates]] =
      Result.fromOption(traverse(_.at(t.toInstant)), s"Tracking not defined over expected region.")

    def map[B](f: A => B): Snapshot[B] =
      copy(base = f(base), asterism = asterism.map(_.map(f)))

    def traverse[F[_]: Applicative, B](f: A => F[B]): F[Snapshot[B]] =
      (f(base), asterism.traverse(pair => pair.traverse(f)))
          .mapN((b, a) => copy(base = b, asterism = a))

  object Snapshot:

    given Traverse[Snapshot] with 
      override def map[A, B](fa: Snapshot[A])(f: A => B): Snapshot[B] = fa.map(f)
      override def foldLeft[A, B](fa: Snapshot[A], b: B)(f: (B, A) => B): B = (fa.base :: fa.asterism.map(_._2)).foldLeft(b)(f)
      override def foldRight[A, B](fa: Snapshot[A], lb: cats.Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = (fa.base :: fa.asterism.map(_._2)).foldRight(lb)(f)
      override def traverse[G[_]: Applicative, A, B](fa: Snapshot[A])(f: A => G[B]): G[Snapshot[B]] = fa.traverse(f)
      
  def instantiate[F[_]: Monad: Temporal: Services: Logger](httpClient: Client[F]): TrackingService[F] =
    new TrackingService:
      
      val horizonsClient: HorizonsClient[F] =
        HorizonsClient(
          httpClient,
          5,        // max retries
          1.second  // initial retry interval
        )

      def getCoordinatesSnapshotOrRegion(
        keys: List[Observation.Id],
        t: Timestamp
      ): F[Map[Observation.Id, Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]]] =
        getTrackingSnapshotOrRegion(keys, TimestampInterval.empty(t))
          .map: resultMap =>
            resultMap
              .view
              .mapValues: res =>
                res.flatMap:
                  case Left(ts) => ts.at(t).map(Left(_))
                  case Right(r) => Result.success(Right(r))            
              .toMap

      def getTrackingSnapshotOrRegion(
        oid: Observation.Id, 
        interval: TimestampInterval
      ): F[Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]] =
        getTrackingSnapshotOrRegion(List(oid), interval).map(_(oid))
      
      def getTrackingSnapshot(
        oid: Observation.Id, 
        interval: TimestampInterval
      ): F[Result[Snapshot[Tracking]]] =
        getTrackingSnapshotOrRegion(oid, interval)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getCoordinatesSnapshotOrRegion(
        oid: Observation.Id, 
        t: Timestamp
      ): F[Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]] =
        getCoordinatesSnapshotOrRegion(List(oid), t).map(_(oid))


      def getCoordinatesSnapshot(
        oid: Observation.Id, 
        t: Timestamp
      ): F[Result[Snapshot[Coordinates]]]  =
        getCoordinatesSnapshotOrRegion(oid, t)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getTrackingSnapshotOrRegion(
        keys: List[Observation.Id],
        interval: TimestampInterval
      ): F[Map[Observation.Id, Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]]] =
        getSiteAndExplicitBaseCoordinates(keys).flatMap: (explicitBases, sites) =>
          asSuperUser: // hm
            asterismService
              .getAsterisms(keys) // TODO: even though this is a bulk fetch it's still inefficient; we don't need all this information
              .flatMap: asterismMap =>
                keys
                  .traverse: 
                    case oid => 
                      asterismMap
                        .get(oid)
                        .fold(OdbError.InvalidObservation(oid, Some(s"No targets are defined for $oid.")).asFailureF): ts =>
                          NonEmptyList.fromList(ts) match
                            case None      => OdbError.InvalidObservation(oid, Some(s"No targets are defined for $oid.")).asFailureF
                            case Some(nel) => mkTracking(oid, sites.get(oid), interval, explicitBases.get(oid), nel)
                        .map(oid -> _)
                  .map(_.toMap)

      private def getSiteAndExplicitBaseCoordinates(oids: List[Observation.Id]): F[(Map[Observation.Id, Coordinates], Map[Observation.Id, Site])] =
        NonEmptyList.fromList(oids.distinct) match
          case None => (Map.empty, Map.empty).pure[F]
          case Some(nel) =>
            session
              .prepareR(Statements.selectSiteAndExplicitBaseCoordinates(nel))
              .use: pq =>
                pq.stream(nel, 1024)
                  .compile
                  .toList
                  .map: list =>
                    val a = list.collect { case (oid, _, Some(x)) => oid -> x } .toMap
                    val b = list.collect { case (oid, Some(x), _) => oid -> x } .toMap
                    (a, b)

      private def mkTracking(
        oid: Observation.Id,
        site: Option[Site],
        interval: TimestampInterval,
        explicitBase: Option[Coordinates],
        asterism: NonEmptyList[(Target.Id, Target)]
      ): F[Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]] =
        asterism 
          .traverse: (tid, target) =>
            target match
              case Nonsidereal(_, key, _)      => mkEphemerisTracking(tid, key, site, interval).map(eph => tid -> eph.asRight)
              case Sidereal(_, tracking, _, _) => ResultT.success(tid -> tracking.asRight)
              case Opportunity(_, region, _)   => ResultT.success(tid -> region.asLeft)
          .map(_.traverse(_.sequence)) // these are not the droids you are looking for
          .map:
            case Left(r)   => Right((r, explicitBase))
            case Right(ts) => Left(Snapshot(oid, explicitBase.fold(CompositeTracking(ts.map(_._2)))(ConstantTracking.apply), ts))
          .value
            
      private def mkEphemerisTracking(tid: Target.Id, key: EphemerisKey, site: Option[Site], interval: TimestampInterval): ResultT[F, EphemerisTracking] =
        (key, site) match
          case (h: EphemerisKey.Horizons, Some(site)) => mkHorizonsEphemerisTracking(h, site, interval)
          case (h: EphemerisKey.Horizons, None)       => ResultT.failure(OdbError.InvalidTarget(tid, s"Cannot determine site for $tid ephemeris.".some).asProblem)
          case (u: EphemerisKey.UserSupplied, _)      => ResultT.failure(OdbError.InvalidTarget(tid, s"Target $tid has a user-defined ephemeris key (not implemented yet).".some).asProblem)
        
      private def mkHorizonsEphemerisTracking(key: EphemerisKey.Horizons, site: Site, interval: TimestampInterval): ResultT[F, EphemerisTracking] =
        loadHorizonsEphemeris(key, site, interval).flatMap:
          case Some(eph) => ResultT.success(eph.ephemerisTracking)
          case None => 
            fetchHorizonsEphemeris(key, site, interval)
              .flatTap(cacheHorizonsEphemeris)
              .map(_.ephemerisTracking)

      private def fetchHorizonsEphemeris(key: EphemerisKey.Horizons, site: Site, interval: TimestampInterval): ResultT[F, HorizonsEphemeris] =
        ResultT:
          horizonsClient
            .alignedEphemeris(key, site, interval.start.toInstant, interval.days, interval.cadence)
            .map(Result.fromEither)
          
      @nowarn("msg=unused")
      private def cacheHorizonsEphemeris(eph: HorizonsEphemeris): ResultT[F, Unit] =
        ResultT.unit // TODO

      @nowarn("msg=unused")
      private def loadHorizonsEphemeris(key: EphemerisKey.Horizons, site: Site, interval: TimestampInterval): ResultT[F, Option[HorizonsEphemeris]] =
        ResultT.success(None) // TODO

  object Statements:

      def selectSiteAndExplicitBaseCoordinates(oids: NonEmptyList[Observation.Id]): Query[oids.type, (Observation.Id, Option[Site], Option[Coordinates])] =
        sql"""
          SELECT c_observation_id, c_observing_mode_type, c_explicit_ra, c_explicit_dec
          FROM t_observation
          WHERE c_observation_id IN (${observation_id.nel(oids)})
        """
          .query(observation_id *: observing_mode_type.opt *: right_ascension.opt *: declination.opt)
          .map:
            case (oid, omode, ora, odec) => 
              val osite: Option[Site] =
                omode.map:
                    case ObservingModeType.GmosNorthLongSlit  => Site.GN
                    case ObservingModeType.GmosSouthLongSlit  => Site.GS
                    case ObservingModeType.Flamingos2LongSlit => Site.GS
                    case ObservingModeType.GmosNorthImaging   => Site.GN
                    case ObservingModeType.GmosSouthImaging   => Site.GS                  
              (oid, osite, (ora, odec).mapN(Coordinates.apply))