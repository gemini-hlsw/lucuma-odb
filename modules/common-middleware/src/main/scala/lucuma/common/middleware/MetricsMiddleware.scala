// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.*
import cats.data.Kleisli
import cats.effect.Clock
import cats.effect.Concurrent
import org.http4s.HttpRoutes
import org.http4s.headers.Upgrade
import org.http4s.metrics.MetricsOps
import org.http4s.server.middleware.Metrics

object MetricsMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * A middleware that records HTTP metrics, skipping WebSocket upgrade requests since they can be
   * very long lived (and would otherwise skew the metrics).
   */
  def httpMetrics[F[_]: {Concurrent, Clock}](metricsOps: MetricsOps[F]): Middleware[F] = routes =>
    val withMetrics = Metrics[F](metricsOps)(routes)
    Kleisli(req => if req.headers.get[Upgrade].isDefined then routes(req) else withMetrics(req))
