// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import lucuma.core.model.Observation
import lucuma.odb.graphql.input.TimingWindowInput
import edu.gemini.grackle.Result
import skunk.AppliedFragment
import skunk.syntax.all.*
import lucuma.odb.util.Codecs.*
import skunk.Transaction
import cats.effect.Sync
import skunk.Session
import cats.syntax.all.*

trait TimingWindowService[F[_]] {
  def createFunction(
    timingWindows: List[TimingWindowInput]
  ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]]
}

object TimingWindowService:
  def fromSession[F[_]: Sync](
    session: Session[F]
  ): TimingWindowService[F] =
    new TimingWindowService[F] {
      private def exec(af: AppliedFragment): F[Unit] =
        session.prepareR(af.fragment.command).use { pq =>
          pq.execute(af.argument).void
        }

      override def createFunction(
        timingWindows: List[TimingWindowInput]
      ): Result[(List[Observation.Id], Transaction[F]) => F[Unit]] =
        Result( (obsIds, xa) =>
          exec(Statements.setObservationTimingWindows(obsIds.head, timingWindows))
        )
    }

object Statements {
  // TODO Handle multiple ObsIds
  def setObservationTimingWindows(
    observationId: Observation.Id,
    timingWindows: List[TimingWindowInput]
  ): AppliedFragment =
    // DELETE FROM t_timing_window WHERE observation_id = $observation_id;
    sql"""
      INSERT INTO t_timing_window(
        c_observation_id,
        c_inclusion,
        c_start,
        c_end_at
      ) VALUES ${(
        observation_id          ~
        timing_window_inclusion ~
        core_timestamp          ~
        core_timestamp.opt
      ).values.list(timingWindows.length)}
    """//.apply(observationId ~ timingWindows.map(tw => observationId ~ tw.inclusion ~ tw.start))
    .apply(timingWindows.map(tw => observationId ~ tw.inclusion ~ tw.start ~ tw.end.flatMap(_.endAt)))
}