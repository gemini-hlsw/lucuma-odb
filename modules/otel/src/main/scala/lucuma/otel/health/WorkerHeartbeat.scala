// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.otel.health

import cats.effect.Clock
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Sync
import cats.syntax.all.*
import org.typelevel.otel4s.Attribute
import org.typelevel.otel4s.metrics.Meter

/**
 * Liveness for worker dynos, which serve no HTTP. The gauge holds the epoch
 * second of the last completed loop iteration, whether or not there was work,
 * so Grafana can alert on staleness. Thresholds live in Grafana, not here.
 */
object WorkerHeartbeat:

  val MetricName: String = "lucuma.worker.last_iteration_epoch_seconds"

  /** Registers the gauge and yields the action a loop calls once per iteration. */
  def resource[F[_]: Sync: Meter](service: String): Resource[F, F[Unit]] =
    Resource.eval(Ref[F].of(Option.empty[Long])).flatMap: last =>
      Meter[F]
        .observableGauge[Long](MetricName)
        .withUnit("s")
        .withDescription("Epoch second of the last completed worker loop iteration")
        .createWithCallback: cb =>
          last.get.flatMap(_.traverse_(cb.record(_, Attribute("service", service))))
        .as(Clock[F].realTime.flatMap(t => last.set(t.toSeconds.some)))
