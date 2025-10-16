// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.Eval
import cats.Monad
import cats.Traverse
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Coordinates
import lucuma.core.math.Region
import lucuma.core.model.CompositeTracking
import lucuma.core.model.ConstantTracking
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

  /** A snapshot of information (typically `Tracking` or `Coordinates`) for an observation's base position and asterism. */
  case class Snapshot[+A](oid: Observation.Id, base: A, asterism: NonEmptyList[(Target.Id, A)]):

    /** If this is a tracking snapshot, evaluate it at a given time. */
    def at(t: Timestamp)(using A <:< Tracking): Result[Snapshot[Coordinates]] =
      Result.fromOption(traverse(_(t.toInstant)), "Tracking not defined over expected region.")

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
      
  def instantiate[F[_]: Monad: Temporal: Services: Logger](
    client: Client[F]
  ): TrackingService[F] =
    new TrackingService:
      
      @nowarn // unused for now
      lazy val horizonsClient: HorizonsClient[F] =
        HorizonsClient(client, 5, 1.second)

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
        getExplicitBaseCoordinates(keys).flatMap: explicitBases =>
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
                            case Some(nel) => mkTracking(oid, explicitBases.get(oid), nel)
                        .map(oid -> _)
                  .map(_.toMap)

      private def getExplicitBaseCoordinates(oids: List[Observation.Id]): F[Map[Observation.Id, Coordinates]] =
        NonEmptyList.fromList(oids.distinct) match
          case None => Map.empty.pure[F]
          case Some(nel) =>
            session
              .prepareR(Statements.selectExplicitBaseCoordinates(nel))
              .use: pq =>
                pq.stream(nel, 1024)
                  .compile
                  .toList
                  .map(_.toMap)

      // Temporary, until we have ephimerides plumbed in
      private def mkTracking(oid: Observation.Id, explicitBase: Option[Coordinates], asterism: NonEmptyList[(Target.Id, Target)]): F[Result[Either[Snapshot[Tracking], (Region, Option[Coordinates])]]] =
        asterism
          .traverse: (tid, target) =>
            target match
              case Sidereal(_, tracking, _, _) => EitherT(Result(Right((tid, tracking))))
              case Nonsidereal(_, _, _)        => EitherT(Result.failure(s"Target $tid is nonsidereal (not supported yet)."))
              case Opportunity(_, region, _)   => EitherT(Result(Left(region)))
          .value
          .map:
            case Left(r)   => Right((r, explicitBase))
            case Right(ts) => 
              val base = explicitBase.fold(CompositeTracking(ts.map(_._2)))(ConstantTracking.apply)
              Left(Snapshot(oid, base, ts))
          .pure[F]

        // We want to normalize requests so samples always fall on the hour; and for now we will request one every 4 hours,
        // aligned to midnight UTC.


  object Statements:

      def selectExplicitBaseCoordinates(oids: NonEmptyList[Observation.Id]): Query[oids.type, (Observation.Id, Coordinates)] =
        sql"""
          SELECT c_observation_id, c_explicit_ra, c_explicit_dec
          FROM t_observation
          WHERE c_observation_id IN (${observation_id.nel(oids)})
          AND c_explicit_ra IS NOT NULL
          AND c_explicit_dec IS NOT NULL
        """
          .query(observation_id *: right_ascension *: declination)
          .map:
            case (oid, ra, dec) => 
              (oid, Coordinates(ra, dec))