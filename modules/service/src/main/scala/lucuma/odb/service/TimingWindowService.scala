// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Observation
import lucuma.odb.graphql.input.TimingWindowInput
import lucuma.odb.util.Codecs.*
import skunk.AppliedFragment
import skunk.Transaction
import skunk.codec.numeric.*
import skunk.syntax.all.*

import Services.Syntax.*

trait TimingWindowService[F[_]] {
  def createFunction(
    timingWindows: List[TimingWindowInput]
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]

  def cloneTimingWindows(
    originalId: Observation.Id,
    newId: Observation.Id,
  )(using Transaction[F]): F[Unit]
}

object TimingWindowService:
  def instantiate[F[_]: MonadCancelThrow](using Services[F]): TimingWindowService[F] =
    new TimingWindowService[F] {

      override def createFunction(
        timingWindows: List[TimingWindowInput]
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        Result( (obsIds, _) =>
          session.exec(Statements.deleteObservationsTimingWindows(obsIds)) >>
            Statements.createObservationsTimingWindows(obsIds, timingWindows).fold(().pure[F])(session.exec)
        )

      def cloneTimingWindows(
        originalId: Observation.Id,
        newId: Observation.Id,
      )(using Transaction[F]): F[Unit] =
        session.exec(Statements.clone(originalId, newId))
    }

object Statements {
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
