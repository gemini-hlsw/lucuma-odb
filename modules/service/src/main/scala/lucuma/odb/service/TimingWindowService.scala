// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.model.Observation
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.input.TimingWindowInput
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.AppliedFragment
import skunk.Query
import skunk.Transaction
import skunk.codec.numeric.*
import skunk.syntax.all.*

import Services.Syntax.*

trait TimingWindowService[F[_]] {
  def createFunction(
    timingWindows: List[TimingWindowInput]
  )(using SuperUserAccess): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def cloneTimingWindows(
    originalId: Observation.Id,
    newId: Observation.Id,
  )(using Transaction[F], SuperUserAccess): F[Unit]
}

object TimingWindowService:
  def instantiate[F[_]: MonadCancelThrow](using Services[F]): TimingWindowService[F] =
    new TimingWindowService[F] {

      override def createFunction(
        timingWindows: List[TimingWindowInput]
      )(using SuperUserAccess): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        Result( (obsIds, _) =>
          stampChanged(obsIds, timingWindows) >>
            session.exec(Statements.deleteObservationsTimingWindows(obsIds)) >>
            Statements.createObservationsTimingWindows(obsIds, timingWindows).fold(().pure[F])(session.exec)
        )

      /**
       * Moves the point an observation's scheduling availability is measured
       * from, for those observations whose windows this edit actually changes.
       *
       * Re-stamping unconditionally would punish a PI for saving the same windows
       * again: the measurement would restart from today and the observation could
       * fall short of a minimum it has met all along.  Windows the database added
       * on the observation's behalf are ignored, since they are not the PI
       * declaring anything.
       */
      private def stampChanged(
        obsIds:  List[Observation.Id],
        incoming: List[TimingWindowInput]
      ): F[Unit] =
        if obsIds.isEmpty then ().pure[F]
        else
          val wanted = Statements.multiset(incoming.map(Statements.keyOf))
          session
            .execute(Statements.SelectDeclaredWindows(observation_id.list(obsIds.length)))(obsIds)
            .map: rows =>
              val existing = rows.groupMap(_._1)(_._2)
              obsIds.filter(oid => Statements.multiset(existing.getOrElse(oid, Nil)) =!= wanted)
            .flatMap:
              case Nil     => ().pure[F]
              case changed => session.exec(Statements.stampAvailabilityAnchor(changed))

      def cloneTimingWindows(
        originalId: Observation.Id,
        newId: Observation.Id,
      )(using Transaction[F], SuperUserAccess): F[Unit] =
        session.exec(Statements.clone(originalId, newId))
    }

object Statements {

  /** Everything about a window that the PI chose, and so everything a change can consist of. */
  type WindowKey = (TimingWindowInclusion, Timestamp, Option[Timestamp], Option[TimeSpan], Option[TimeSpan], Option[Int])

  def keyOf(tw: TimingWindowInput): WindowKey =
    (
      tw.inclusion,
      tw.startUtc,
      tw.end.flatMap(_.atUtc),
      tw.end.flatMap(_.after),
      tw.end.flatMap(_.repeat.map(_.period)),
      tw.end.flatMap(_.repeat.flatMap(_.times.map(_.value)))
    )

  /** Order is not part of a window set's identity, but multiplicity is. */
  def multiset(keys: List[WindowKey]): Map[WindowKey, Int] =
    keys.groupMapReduce(identity)(_ => 1)(_ + _)

  def SelectDeclaredWindows(enc: skunk.Encoder[List[Observation.Id]]): Query[List[Observation.Id], (Observation.Id, WindowKey)] =
    sql"""
      SELECT
        c_observation_id,
        c_inclusion,
        c_start,
        c_end_at,
        c_end_after,
        c_repeat_period,
        c_repeat_times
      FROM t_timing_window
      WHERE NOT c_automatic
        AND c_observation_id IN ($enc)
    """
    .query(observation_id *: timing_window_inclusion *: core_timestamp *: core_timestamp.opt *: time_span.opt *: time_span.opt *: int4.opt)
    .map { case (oid, i, s, ea, af, rp, rt) => (oid, (i, s, ea, af, rp, rt)) }

  def stampAvailabilityAnchor(observationIds: List[Observation.Id]): AppliedFragment =
    sql"""
      UPDATE t_observation
         SET c_availability_anchor = now()
       WHERE c_observation_id IN ${observation_id.list(observationIds.length).values}
    """.apply(observationIds)

  def deleteObservationsTimingWindows(
    observationIds: List[Observation.Id]
  ): AppliedFragment =
    sql"""
      DELETE FROM t_timing_window
      WHERE c_observation_id IN ${observation_id.list(observationIds.length).values}
    """.apply(observationIds)

  def createObservationsTimingWindows(
    observationIds: List[Observation.Id],
    timingWindows: List[TimingWindowInput]
  ): Option[AppliedFragment] =
    (observationIds, timingWindows) match
      case (Nil, _) => none
      case (_, Nil) => none
      case _ =>
        sql"""
          INSERT INTO t_timing_window (
            c_observation_id,
            c_inclusion,
            c_start,
            c_end_at,
            c_end_after,
            c_repeat_period,
            c_repeat_times
          ) VALUES ${(
            observation_id          *:
            timing_window_inclusion *:
            core_timestamp          *:
            core_timestamp.opt      *:
            time_span.opt           *:
            time_span.opt           *:
            int4.opt
          ).values.list(timingWindows.length).list(observationIds.length)}
        """
        .apply(
          observationIds.map( obsId =>
            timingWindows.map { tw => (
              obsId ,
              tw.inclusion  ,
              tw.startUtc      ,
              tw.end.flatMap(_.atUtc) ,
              tw.end.flatMap(_.after) ,
              tw.end.flatMap(_.repeat.map(_.period)) ,
              tw.end.flatMap(_.repeat.flatMap(_.times.map(_.value)))
            )}
          )
        ).some

  def clone(originalOid: Observation.Id, newOid: Observation.Id): AppliedFragment =
    sql"""
      INSERT INTO t_timing_window (
        c_observation_id,
        c_inclusion,
        c_start,
        c_end_at,
        c_end_after,
        c_repeat_period,
        c_repeat_times
      )
      SELECT
        $observation_id,
        t_timing_window.c_inclusion,
        t_timing_window.c_start,
        t_timing_window.c_end_at,
        t_timing_window.c_end_after,
        t_timing_window.c_repeat_period,
        t_timing_window.c_repeat_times
      FROM t_timing_window
      WHERE c_observation_id = $observation_id
    """.apply(newOid, originalOid)
}
