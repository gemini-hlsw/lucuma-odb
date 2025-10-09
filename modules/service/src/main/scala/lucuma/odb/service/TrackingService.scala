// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.model.Tracking
import cats.data.NonEmptyList
import lucuma.core.math.Region
import cats.Monad
import cats.syntax.all.*
import lucuma.core.util.TimestampInterval
import lucuma.core.util.Timestamp
import lucuma.core.model.Program
import lucuma.core.model.Observation
import grackle.Result
import lucuma.core.math.Coordinates
import lucuma.core.model.Target
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import cats.Applicative
import cats.Traverse
import cats.Eval
import lucuma.odb.service.Services.Syntax.asterismService
import lucuma.odb.service.Services.asSuperUser
import lucuma.core.model.Target.Opportunity
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.Target.Nonsidereal
import cats.data.EitherT
import lucuma.core.model.CompositeTracking

trait TrackingService[F[_]]:
  import TrackingService.Snapshot

  /**
   * This is the batch version of `getTrackingSnapshotOrRegion`. Each passed key is guaranteed to
   * exist in the yielded map.
   */
  def getTrackingSnapshotOrRegion(
    keys: List[(Program.Id, Observation.Id)],
    interval: TimestampInterval
  ): F[Map[(Program.Id, Observation.Id), Result[Either[Snapshot[Tracking], Region]]]]

  /**
   * This is the batch version of `getCoordinatesSnapshotOrRegion`. Each passed key is guaranteed to
   * exist in the yielded map.
   */
  def getCoordinatesSnapshotOrRegion(
    keys: List[(Program.Id, Observation.Id)],
    t: Timestamp
  ): F[Map[(Program.Id, Observation.Id), Result[Either[Snapshot[Coordinates], Region]]]]

  /**
   * Yield the asterism's base tracking (an explicit base position, if defined, otherwise the centroid
   * of the component targets), as well as the component tracking for each science targett;
   * or the region if the asterism contains any opportunity targets. The returned ephemerides are 
   * guaranteed to be defined over `interval` but may not be defined outside it. All other methods on
   * this interface are defined in terms of this operation.
   * 
   * For nonsidereal targets that require ephemeris information from HORIZONS we always select at
   * least one point outside `interval` on each end to guarantee that coordinates are known at 
   * `interval`'s endpoints. For intervals shorter than 24h we select one point per hour, and for
   * larger intervals we select one point per 12h. These points are aligned to UTC on the hour to
   * facilitate caching.
   * 
   * This method will yield a failed result if the asterism is empty, if there are nonsidereal targets 
   * and the site can't be determined, if an ephemeris can't be fetched due to a service error, or 
   * if a user-defined ephemeris isn't defined for the specified interval.
   */
  def getTrackingSnapshotOrRegion(
    pid: Program.Id, 
    oid: Observation.Id, 
    interval: TimestampInterval
  ): F[Result[Either[Snapshot[Tracking], Region]]] 

  def getTrackingSnapshot(
    pid: Program.Id, 
    oid: Observation.Id, 
    interval: TimestampInterval
  ): F[Result[Snapshot[Tracking]]] 


  /**
   * Equivalent to `getTrackingSnapshotOrRegion` when computed for an interval around `t` and evaluated
   * at `t`.
   */
  def getCoordinatesSnapshotOrRegion(
    pid: Program.Id, 
    oid: Observation.Id, 
    t: Timestamp
  ): F[Result[Either[Snapshot[Coordinates], Region]]] 

  def getCoordinatesSnapshot(
    pid: Program.Id, 
    oid: Observation.Id, 
    t: Timestamp
  ): F[Result[Snapshot[Coordinates]]] 




object TrackingService:

  /** A snapshot of information (typically `Tracking` or `Coordinates`) for an observation's base position and asterism. */
  case class Snapshot[+A](pid: Program.Id, oid: Observation.Id, base: A, asterism: NonEmptyList[(Target.Id, A)]):

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
      
  def instantiate[F[_]: Monad: Services]: TrackingService[F] =
    new TrackingService:
        
      def getCoordinatesSnapshotOrRegion(
        keys: List[(Program.Id, Observation.Id)],
        t: Timestamp
      ): F[Map[(Program.Id, Observation.Id), Result[Either[Snapshot[Coordinates], Region]]]] =
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
        pid: Program.Id, 
        oid: Observation.Id, 
        interval: TimestampInterval
      ): F[Result[Either[Snapshot[Tracking], Region]]] =
        val key = (pid, oid)
        getTrackingSnapshotOrRegion(List(key), interval).map(_(key))
      
      def getTrackingSnapshot(
        pid: Program.Id, 
        oid: Observation.Id, 
        interval: TimestampInterval
      ): F[Result[Snapshot[Tracking]]] =
        getTrackingSnapshotOrRegion(pid, oid, interval)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getCoordinatesSnapshotOrRegion(
        pid: Program.Id, 
        oid: Observation.Id, 
        t: Timestamp
      ): F[Result[Either[Snapshot[Coordinates], Region]]] =
        val key = (pid, oid)
        getCoordinatesSnapshotOrRegion(List(key), t).map(_(key))


      def getCoordinatesSnapshot(
        pid: Program.Id, 
        oid: Observation.Id, 
        t: Timestamp
      ): F[Result[Snapshot[Coordinates]]]  =
        getCoordinatesSnapshotOrRegion(pid, oid, t)
          .map: res =>
            res.flatMap:
              case Left(t)  => Result.success(t)
              case Right(r) => OdbError.InvalidObservation(oid, Some(s"Tracking unavailable for $oid due to opportunity targets.")).asFailure

      def getTrackingSnapshotOrRegion(
        keys: List[(Program.Id, Observation.Id)],
        interval: TimestampInterval
      ): F[Map[(Program.Id, Observation.Id), Result[Either[Snapshot[Tracking], Region]]]] =
        asSuperUser: // hm
          asterismService
            .getAsterisms(keys.map(_._2)) // TODO: even though this is a bulk fetch it's still inefficient; we don't need all this information
            .flatMap: asterismMap =>
              keys
                .traverse: 
                  case key @ (pid, oid) => 
                    asterismMap
                      .get(oid)
                      .fold(OdbError.InvalidObservation(oid).asFailureF): ts =>
                        NonEmptyList.fromList(ts) match
                          case None      => OdbError.InvalidObservation(oid, Some(s"No targets are defined for $oid.")).asFailureF
                          case Some(nel) => mkTracking(pid, oid, nel)
                      .map(key -> _)
                .map(_.toMap)

      // Temporary, until we have ephimerides plumbed in
      private def mkTracking(pid: Program.Id, oid: Observation.Id, asterism: NonEmptyList[(Target.Id, Target)]): F[Result[Either[Snapshot[Tracking], Region]]] =
        asterism
          .traverse: (tid, target) =>
            target match
              case Sidereal(_, tracking, _, _) => EitherT(Result(Right((tid, tracking))))
              case Nonsidereal(_, _, _)        => EitherT(Result.failure(s"Target $tid is nonsidereal (not supported yet)."))
              case Opportunity(_, region, _)   => EitherT(Result(Left(region)))
          .value
          .map:
            case Left(r)   => Right(r)
            case Right(ts) => Left(Snapshot(pid, oid, CompositeTracking(ts.map(_._2)), ts))
          .pure[F]

