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
import lucuma.core.data.Metadata
import lucuma.core.data.PerSite
import lucuma.core.enums.EphemerisKeyType
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.math.Region
import lucuma.core.model.AirMass
import lucuma.core.model.AirMassBound
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
import lucuma.core.model.Ephemeris
import lucuma.core.model.Ephemeris.Horizons
import lucuma.core.model.Ephemeris.UserSupplied
import lucuma.core.model.EphemerisTracking
import lucuma.core.model.Extinction
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Opportunity
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.Tracking
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.horizons.HorizonsClient
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.Services.asSuperUser
import lucuma.odb.util.Codecs.*
import skunk.Command
import skunk.Encoder
import skunk.Query
import skunk.codec.all.*
import skunk.syntax.all.*

import java.time.Instant
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.*
import lucuma.odb.service.Services.SuperUserAccess

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
    interval: TimestampInterval,
    force: Boolean,
  ): F[Map[Observation.Id, Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]]]

  /**
   * Convenience method that calls `getTrackingSnapshotOrRegion` over an interval around `t` and
   * evaluates any computed tracking functions at that point.
   */
  def getCoordinatesSnapshotOrRegion(
    keys: List[Observation.Id],
    t: Timestamp,
    force: Boolean,
  ): F[Map[Observation.Id, Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]]]

  /** Single-target version of `getCoordinatesSnapshotOrRegion`. */
  def getTrackingSnapshotOrRegion(
    oid: Observation.Id, 
    interval: TimestampInterval,
    force: Boolean,
  ): F[Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]] 

  /** 
   * Convenience method that calls `getTrackingSnapshotOrRegion` that discards any resulting
   * region and turns it into a failure.
   */
  def getTrackingSnapshot(
    oid: Observation.Id, 
    interval: TimestampInterval,
    force: Boolean,
  ): F[Result[Snapshot[Tracking]]] 

  /** Single-target version of `getCoordinatesSnapshotOrRegion`. */
  def getCoordinatesSnapshotOrRegion(
    oid: Observation.Id, 
    t: Timestamp,
    force: Boolean,
  ): F[Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]] 

  /** 
   * Convenience method that calls `getCoordinatesSnapshotOrRegion` that discards any resulting
   * region and turns it into a failure.
   */
  def getCoordinatesSnapshot(
    oid: Observation.Id, 
    t: Timestamp,
    force: Boolean,
  ): F[Result[Snapshot[Coordinates]]] 

  /** Create a new user-supplied ephemeris. */
  def createUserSuppliedEphemeris(
    elements: PerSite[List[Ephemeris.UserSupplied.Element]]
  )(using SuperUserAccess): F[Result[Ephemeris.Key.UserSupplied]]

  /** 
   * Replace a user-supplied ephemeris. Calling user must have edit permission for the
   * associated program. 
   */
  def replaceUserSuppliedEphemeris(
    ephemeris: Ephemeris.UserSupplied
  )(using SuperUserAccess): F[Result[Unit]]

  def deleteUserSuppliedEphemeris(
    key: Ephemeris.Key.UserSupplied
  )(using SuperUserAccess): F[Result[Unit]]

object TrackingService:

  /** Whitebox interface for testing, available by downcasting. */
  trait Whitebox[F[_]] extends TrackingService[F]:
    def getTrackingSnapshotEx(
      oid: Observation.Id, 
      interval: TimestampInterval,
      force: Boolean
    ): F[Result[Snapshot[(Tracking, Int)]]] 

  extension (interval: TimestampInterval) 

    private def days: Int = 
      interval.duration.toDays.toInt max 1 // always at least one day

    private def cadence: HorizonsClient.ElementsPerDay =
      import interval.duration
      if      duration <=  1.day then 24
      else if duration <=  2.day then 12
      else if duration <=  3.day then 8
      else if duration <=  4.day then 6
      else if duration <=  6.day then 4
      else if duration <= 12.day then 2
      else 1

    def alignedZonedDateTime =
      ZonedDateTime
        .ofInstant(interval.start.toInstant, ZoneOffset.UTC)
        .withHour(0)
        .withMinute(0)
        .withSecond(0)
        .withNano(0)   

    def alignedStart: Timestamp =
      Timestamp.fromInstantTruncatedAndBounded:
        alignedZonedDateTime.toInstant()

    def alignedEnd: Timestamp =
      Timestamp.fromInstantTruncatedAndBounded:
        alignedZonedDateTime.plusDays(days).toInstant()

    def expectedAlignedElements = 
      days * cadence

    def expectedInstants: List[Instant] =
      (0 to expectedAlignedElements).toList.map: n =>
          alignedZonedDateTime.plusHours(n * 24 / cadence).toInstant()

  extension [F[_]: Monad, A](rt: ResultT[F, A]) def flatTap(f: A => ResultT[F, Unit]): ResultT[F, A] =
    rt.flatMap(a => f(a).as(a))

  /** A snapshot of information (typically `Tracking` or `Coordinates`) for an observation's base position and asterism. */
  case class Snapshot[+A](oid: Observation.Id, base: A, asterism: NonEmptyList[(Target.Id, A)]):

    /** If this is a tracking snapshot, evaluate it at a given time. */
    def at(t: Timestamp)(using A <:< Tracking): Result[Snapshot[Coordinates]] =
      Result.fromOption(traverse(_.at(t.toInstant)), s"Tracking not defined over expected region.")

    def atEx(t: Timestamp)(using A <:< (Tracking, Int)): Result[Snapshot[(Coordinates, Int)]] =
      Result.fromOption(traverse(p => p._1.at(t.toInstant).tupleRight(p._2)), s"Tracking not defined over expected region.")

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
      
  def instantiate[F[_]: Monad: Temporal: Services](horizonsClient: HorizonsClient[F]): TrackingService[F] =
    new Whitebox:      

      def getCoordinatesSnapshotOrRegion(
        keys: List[Observation.Id],
        t: Timestamp,
        force: Boolean,
      ): F[Map[Observation.Id, Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]]] =
        getTrackingSnapshotOrRegion(keys, TimestampInterval.empty(t), force)
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
        interval: TimestampInterval,
        force: Boolean,
      ): F[Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]] =
        getTrackingSnapshotOrRegion(List(oid), interval, force).map(_(oid))
      
      def getTrackingSnapshotOrRegionEx(
        oid: Observation.Id, 
        interval: TimestampInterval,
        force: Boolean,
      ): F[Result[Either[Snapshot[(Tracking, Int)], (Region, Option[Coordinates])]]] =
        getTrackingSnapshotOrRegionEx(List(oid), interval, force).map(_(oid))

      def getTrackingSnapshotEx(
        oid: Observation.Id, 
        interval: TimestampInterval,
        force: Boolean
      ): F[Result[Snapshot[(Tracking, Int)]]] =
        getTrackingSnapshotOrRegionEx(oid, interval, force)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getTrackingSnapshot(
        oid: Observation.Id, 
        interval: TimestampInterval,
        force: Boolean,
      ): F[Result[Snapshot[Tracking]]] =
        getTrackingSnapshotOrRegion(oid, interval, force)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getCoordinatesSnapshotOrRegion(
        oid: Observation.Id, 
        t: Timestamp,
        force: Boolean,
      ): F[Result[Either[Snapshot[Coordinates], (Region, Option[Coordinates])]]] =
        getCoordinatesSnapshotOrRegion(List(oid), t, force).map(_(oid))

      def getCoordinatesSnapshot(
        oid: Observation.Id, 
        t: Timestamp,
        force: Boolean,
      ): F[Result[Snapshot[Coordinates]]]  =
        getCoordinatesSnapshotOrRegion(oid, t, force)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getTrackingSnapshotOrRegion(
        keys: List[Observation.Id],
        interval: TimestampInterval,
        force: Boolean,
      ): F[Map[Observation.Id, Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]]] =
        getTrackingSnapshotOrRegionEx(keys, interval, force)
          .map: m =>              
            m.map: p =>           
              p.map: r =>         
                r.map: e =>       
                  e.leftMap: s => 
                    s.map(_._1)    // \o/

      def getTrackingSnapshotOrRegionEx(
        keys: List[Observation.Id],
        interval: TimestampInterval,
        force: Boolean
      ): F[Map[Observation.Id, Result[Either[Snapshot[(Tracking, Int)], (Region, Option[Coordinates])]]]] =
        getSiteAndExplicitBaseCoordinates(keys, interval).flatMap: (explicitBases, sites) =>
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
                            case Some(nel) => mkTrackingEx(oid, sites.get(oid), interval, explicitBases.get(oid), nel, force)
                        .map(oid -> _)
                  .map(_.toMap)

      def replaceUserSuppliedEphemeris(
        ephemeris: Ephemeris.UserSupplied
      )(using SuperUserAccess): F[Result[Unit]] =
        (deleteEphemeris(ephemeris.key) *> storeEphemeris(ephemeris)).value

      def createUserSuppliedEphemeris(
        elements: PerSite[List[Ephemeris.UserSupplied.Element]]
      )(using SuperUserAccess): F[Result[Ephemeris.Key.UserSupplied]] =
        ResultT
          .liftF(session.unique(Statements.CreateUserSuppliedEphemerisKey))
          .map(UserSupplied(_, elements))
          .flatTap(storeEphemeris(_))
          .map(_.key)
          .value
              
      def deleteUserSuppliedEphemeris(
        key: Ephemeris.Key.UserSupplied
      )(using SuperUserAccess): F[Result[Unit]] =
        deleteEphemeris(key).value

      private def getSiteAndExplicitBaseCoordinates(oids: List[Observation.Id], when: TimestampInterval): F[(Map[Observation.Id, Coordinates], Map[Observation.Id, Site])] =
        NonEmptyList.fromList(oids.distinct) match
          case None => (Map.empty, Map.empty).pure[F]
          case Some(nel) =>
            session
              .prepareR(Statements.selectSiteAndExplicitBaseCoordinates(nel, when, metadata))
              .use: pq =>
                pq.stream(nel, 1024)
                  .compile
                  .toList
                  .map: list =>
                    val a = list.collect { case (oid, _, Some(x)) => oid -> x } .toMap
                    val b = list.collect { case (oid, Some(x), _) => oid -> x } .toMap
                    (a, b)
            
      private def mkTrackingEx(
        oid: Observation.Id,
        site: Option[Site],
        interval: TimestampInterval,
        explicitBase: Option[Coordinates],
        asterism: NonEmptyList[(Target.Id, Target)],
        force: Boolean
      ): F[Result[Either[Snapshot[(Tracking, Int)], (Region, Option[Coordinates])]]] =
        asterism 
          .traverse: (tid, target) =>
            val p = 
              target match
                case Nonsidereal(_, key, _)      => mkEphemerisTrackingEx(tid, key, site, interval, force).map(eph => tid -> eph.asRight)
                case Sidereal(_, tracking, _, _) => ResultT.success(tid -> tracking.asRight.tupleRight(0))
                case Opportunity(_, region, _)   => ResultT.success(tid -> region.asLeft.tupleRight(0))
            p.widen[(Target.Id, Either[Region, (Tracking, Int)])] // :-\
          .map(_.traverse(_.sequence)) // these are not the droids you are looking for
          .map:
            case Left(r)   => Right((r, explicitBase))
            case Right(ts) => 
              val composite = (CompositeTracking(ts.map(_._2._1)), ts.foldMap(_._2._2))
              Left(Snapshot(oid, explicitBase.fold(composite)(b => (ConstantTracking.apply(b), 0)), ts))
          .value

      private def mkEphemerisTrackingEx(tid: Target.Id, key: Ephemeris.Key, site: Option[Site], interval: TimestampInterval, force: Boolean): ResultT[F, (EphemerisTracking, Int)] =
        site match
          case None => ResultT.failure(OdbError.InvalidTarget(tid, s"Cannot determine site for $tid ephemeris.".some).asProblem)
          case Some(site) =>
            key match
              case k: Ephemeris.Key.Horizons     => mkHorizonsEphemerisTracking(k, site, interval, force)
              case k: Ephemeris.Key.UserSupplied => mkUserSuppliedEphemerisTracking(k, site, interval)        

      private def mkHorizonsEphemerisTracking(key: Ephemeris.Key.Horizons, site: Site, interval: TimestampInterval, force: Boolean): ResultT[F, (EphemerisTracking, Int)] =
        val pre = if force then deleteEphemeris(key).as(Left(interval.expectedAlignedElements)) else loadHorizonsEphemeris(key, site, interval)
        pre.flatMap:
          case Right(eph) => ResultT.success((eph.toEphemerisTracking(site), 0))
          case Left(misses) => 
            fetchHorizonsEphemeris(key, interval)
              .flatTap(storeEphemeris)
              .map(a => (a.toEphemerisTracking(site), misses))

      private def mkUserSuppliedEphemerisTracking(key: Ephemeris.Key.UserSupplied, site: Site, interval: TimestampInterval): ResultT[F, (EphemerisTracking, Int)] =
        loadUserSuppliedEphemeris(key, interval).map: e =>
          (e.toEphemerisTracking(site), 0)

      private def fetchHorizonsEphemeris(key: Ephemeris.Key.Horizons, interval: TimestampInterval): ResultT[F, Ephemeris.Horizons] =
        ResultT:
          horizonsClient
            .alignedEphemeris(key, interval.start.toInstant, interval.days, interval.cadence)
            .map(Result.fromEither)
          
      private def storeEphemeris[E <: Ephemeris.Element](eph: Ephemeris[E]): ResultT[F, Unit] =
        ResultT.liftF:
          Statements.StorableEphemerisElement
            .flatten(eph)
            .traverse: es =>
              val stmt = Statements.storeEphemeris(es)
              session.prepareR(stmt).use: ps =>
                ps.execute(es)
            .void

      private def deleteEphemeris(key: Ephemeris.Key): ResultT[F, Unit] =
        ResultT.liftF:
          session.prepareR(Statements.DeleteEphemerisEntries).use: pc =>
            pc.execute(key).void

      private def loadUserSuppliedEphemeris(key: Ephemeris.Key.UserSupplied, interval: TimestampInterval): ResultT[F, Ephemeris.UserSupplied] =
        ResultT.liftF:
          session.prepareR(Statements.SelectUserSuppliedEphemerisEntries).use: pq =>
            pq.stream((key, interval), 1024)
              .compile
              .toList
              .map(PerSite.fromPairs)
              .map: ps =>
                Ephemeris.UserSupplied(key, ps)

      private def loadHorizonsEphemeris(key: Ephemeris.Key.Horizons, site: Site, interval: TimestampInterval): ResultT[F, Either[Int, Ephemeris.Horizons]] =
        ResultT.liftF:
          session.prepareR(Statements.SelectHorizonsEphemerisEntries).use: pq =>
            pq.stream((key, site, interval), 1024)
              .compile
              .toList
              .map: es =>                   
                val instants = es.map(_._2.when).toSet  
                val misses   = interval.expectedInstants.count(i => !instants.contains(i))
                if misses > 0 then
                  Left(misses)
                else
                  Right(Ephemeris.Horizons(key, interval.start.toInstant, interval.end.toInstant, PerSite.fromPairs(es)))

  private object Statements:

    /* This flattens a `Ephemeris` and constrains its `when` values to a storable range. */
    case class StorableEphemerisElement(
      key: Ephemeris.Key,
      site: Site,
      when: Timestamp,
      coordinates: Coordinates,
      velocity: Offset,
      airmass: Option[AirMass],
      extinction: Option[Extinction],
      visualMagnitude: Option[Double],
      surfaceBrightness: Option[Double],
    )
    object StorableEphemerisElement:

      def flatten[E <: Ephemeris.Element](ephemeris: Ephemeris[E]): Option[NonEmptyList[StorableEphemerisElement]] =
        ephemeris
          .elements
          .mapWithSite: (site, elements) =>
            NonEmptyList.fromList:
              elements.flatMap: entry =>
                Timestamp.fromInstant(entry.when).map: when =>
                  entry match
                    case UserSupplied.Element(_, coordinates, velocity) => 
                      StorableEphemerisElement(
                        ephemeris.key,
                        site,
                        when,
                        coordinates,
                        velocity,
                        None,
                        None,
                        None,
                        None
                      )
                    case Horizons.Element(_, coordinates, velocity, airmass, extinction, visualMagnitude, surfaceBrightness) =>
                      StorableEphemerisElement(
                        ephemeris.key,
                        site,
                        when,
                        coordinates,
                        velocity,
                        airmass.filterNot(_.value.value > AirMassBound.Max.value.value.value.value), // Horizons returns airmasses > 3
                        extinction.filterNot(Extinction.FromMilliVegaMagnitude.reverseGet(_) >= 1), // also extinctions > 1
                        Some(visualMagnitude),
                        surfaceBrightness,
                      )
          .combineAll

    def selectSiteAndExplicitBaseCoordinates(oids: NonEmptyList[Observation.Id], when: TimestampInterval, metadata: Metadata): Query[oids.type, (Observation.Id, Option[Site], Option[Coordinates])] =
      sql"""
        SELECT c_observation_id, c_instrument, c_explicit_ra, c_explicit_dec
        FROM t_observation
        WHERE c_observation_id IN (${observation_id.nel(oids)})
      """
        .query(observation_id *: instrument.opt *: right_ascension.opt *: declination.opt)
        .map:
          case (oid, oinst, ora, odec) => 

            val osite: Option[Site] =
              oinst.flatMap: inst =>
                metadata
                  .availability(inst)
                  .siteForRange(cats.collections.Range(when.start.toInstant, when.end.toInstant))
            
            (oid, osite, (ora, odec).mapN(Coordinates.apply))

    def storeEphemeris[A <: NonEmptyList[StorableEphemerisElement]](entries: A): Command[entries.type] = {

      val enc: Encoder[StorableEphemerisElement] =
        ( 
          ephemeris_key_type  *:
          varchar             *:
          site                *:
          core_timestamp      *: 
          right_ascension     *: 
          declination         *: 
          offset              *: 
          air_mass.opt        *: 
          core_extinction.opt *: 
          float8.opt          *: 
          float8.opt
        ).contramap: e =>
          (
            e.key.keyType,
            e.key.des,
            e.site,
            e.when,
            e.coordinates.ra,
            e.coordinates.dec,
            e.velocity,
            e.airmass,
            e.extinction,
            e.visualMagnitude,
            e.surfaceBrightness,
          )

      sql"""
        INSERT INTO t_ephemeris (
          c_key_type,  
          c_des,       
          c_site,      
          c_when,      
          c_ra,        
          c_dec,       
          c_dra,       
          c_ddec,      
          c_airmass,   
          c_extinction,
          c_vmag,      
          c_sb        
        ) VALUES ${enc.values.nel(entries)}
        ON CONFLICT (c_key_type, c_des, c_site, c_when) DO UPDATE SET
          c_ra         = EXCLUDED.c_ra,        
          c_dec        = EXCLUDED.c_dec,       
          c_dra        = EXCLUDED.c_dra,       
          c_ddec       = EXCLUDED.c_ddec,      
          c_airmass    = EXCLUDED.c_airmass,   
          c_extinction = EXCLUDED.c_extinction,
          c_vmag       = EXCLUDED.c_vmag,      
          c_sb         = EXCLUDED.c_sb        
      """.command
    }

    val SelectHorizonsEphemerisEntries: Query[(Ephemeris.Key.Horizons, Site, TimestampInterval), (Site, Ephemeris.Horizons.Element)] =
      sql"""
        SELECT
          c_site,
          c_when,      
          c_ra,        
          c_dec,       
          c_dra,       
          c_ddec,      
          c_airmass,   
          c_extinction,
          c_vmag,      
          c_sb
        FROM  t_ephemeris
        WHERE c_key_type = $ephemeris_key_type
        AND   c_des = $varchar
        AND   c_site = $site
        AND   c_when >= $core_timestamp
        AND   c_when <= $core_timestamp
      """
        .contramap[(Ephemeris.Key.Horizons, Site, TimestampInterval)]: (key, site, interval) =>
          (key.keyType, key.des, site, interval.alignedStart, interval.alignedEnd)
        .query(
          site                *:
          core_timestamp      *: 
          right_ascension     *: 
          declination         *: 
          offset              *: 
          air_mass.opt        *: 
          core_extinction.opt *: 
          numeric             *: 
          numeric.opt
      ).map: (site, ts, ra, dec, v, am, e, vm, sb) =>
        site ->
          Ephemeris.Horizons.Element(
            ts.toInstant, Coordinates(ra, dec), v, am, e, vm.toDouble, sb.map(_.toDouble)
          )

    val SelectUserSuppliedEphemerisEntries: Query[(Ephemeris.Key.UserSupplied, TimestampInterval), (Site, Ephemeris.UserSupplied.Element)] =
      sql"""
        SELECT
          c_site,
          c_when,      
          c_ra,        
          c_dec,       
          c_dra,       
          c_ddec,
        FROM  t_ephemeris
        WHERE c_key_type = $ephemeris_key_type
        AND   c_des = $varchar
        AND   c_when >= $core_timestamp
        AND   c_when <= $core_timestamp
      """
        .contramap[(Ephemeris.Key.UserSupplied, TimestampInterval)]: (key, interval) =>
          (key.keyType, key.des, interval.alignedStart, interval.alignedEnd)
        .query(
          site                *:
          core_timestamp      *: 
          right_ascension     *: 
          declination         *: 
          offset
      ).map: (site, ts, ra, dec, v) =>
        site -> Ephemeris.UserSupplied.Element(ts.toInstant, Coordinates(ra, dec), v)

    val DeleteEphemerisEntries: Command[Ephemeris.Key] =
      sql"""
        DELETE
        FROM  t_ephemeris
        WHERE c_key_type = $ephemeris_key_type
        AND   c_des = $varchar
      """
        .contramap[Ephemeris.Key]: key =>
          (key.keyType, key.des)
        .command

    val CreateUserSuppliedEphemerisKey: Query[skunk.Void, Ephemeris.Key.UserSupplied] =
      sql"""
        SELECT nextval('s_user_supplied_ephemeris_id')
      """.query(int8).map(Ephemeris.Key.UserSupplied.apply)
