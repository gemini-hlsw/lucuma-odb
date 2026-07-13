// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s
import cats.*
import cats.effect.*
import cats.syntax.all.*
import fs2.compression.Compression
import lucuma.common.middleware.CorsMiddleware
import lucuma.common.middleware.ErrorReportingMiddleware
import lucuma.common.middleware.LoggingMiddleware
import lucuma.common.middleware.MetricsMiddleware
import lucuma.common.middleware.TracingMiddleware
import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import org.http4s.HttpRoutes
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware as OtelServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.server.middleware.GZip
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

object ServerMiddleware {
  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * Create a combined middleware to all server routes
   */
  def apply[F[_]: {Async, Compression, Tracer, TracerProvider, MeterProvider}](
    corsOverHttps: Boolean,
    domain:        Seq[String],
    ssoClient:     SsoClient[F, User]
  ): F[Middleware[F]] = {
    given Logger[F] = Slf4jLogger.getLogger[F]

    val logging          = LoggingMiddleware.logging[F]()
    val spanDataProvider = ServerSpanDataProvider.openTelemetry(TracingMiddleware.redactor)
    val cors             = CorsMiddleware.cors[F](corsOverHttps, domain.toList)

    (
      OtelServerMiddleware.builder[F](spanDataProvider).build,
      OtelMetrics.serverMetricsOps[F]().map(MetricsMiddleware.httpMetrics[F])
    ).mapN: (otel, httpMetrics) =>
      List[Middleware[F]](
        cors,
        logging,
        httpMetrics,
        otel.asHttpRoutesMiddleware,
        TracingMiddleware.traceUser(ssoClient),
        ErrorReportingMiddleware.errorReporting,
        GZip(_)
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
  }

}
