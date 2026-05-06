// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s
import cats.*
import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import fs2.compression.Compression
import org.http4s.HttpRoutes
import org.http4s.Query
import org.http4s.Uri
import org.http4s.headers.Upgrade
import org.http4s.metrics.MetricsOps
import org.http4s.otel4s.middleware.metrics.OtelMetrics
import org.http4s.otel4s.middleware.trace.redact
import org.http4s.otel4s.middleware.trace.redact.PathRedactor
import org.http4s.otel4s.middleware.trace.redact.QueryRedactor
import org.http4s.otel4s.middleware.trace.server.ServerMiddleware as OtelServerMiddleware
import org.http4s.otel4s.middleware.trace.server.ServerSpanDataProvider
import org.http4s.server.middleware.ErrorAction
import org.http4s.server.middleware.GZip
import org.http4s.server.middleware.Logger as Http4sLogger
import org.http4s.server.middleware.Metrics
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider

object ServerMiddleware {
  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * Create a combined middleware to all server routes
   */
  def apply[F[_]: {Async, Compression, TracerProvider, MeterProvider}](): F[Middleware[F]] = {
    given Logger[F] = Slf4jLogger.getLogger[F]

    val logging = Http4sLogger.httpRoutes[F](
      logHeaders = true,
      logBody = false
    )

    val httpMetrics: MetricsOps[F] => Middleware[F] = metricsOps =>
      routes =>
        val withMetrics = Metrics[F](metricsOps)(routes)
        Kleisli(req => if (req.headers.get[Upgrade].isDefined) routes(req) else withMetrics(req))

    val errorReporting: Middleware[F] = routes =>
      ErrorAction.httpRoutes.log(
        httpRoutes = routes,
        messageFailureLogAction = Logger[F].error(_)(_),
        serviceErrorLogAction = Logger[F].error(_)(_)
      )
    val spanDataProvider              = ServerSpanDataProvider.openTelemetry(redactor)

    (
      OtelServerMiddleware.builder[F](spanDataProvider).build,
      OtelMetrics.serverMetricsOps[F]()
    ).mapN: (otel, metricsOps) =>
      List[Middleware[F]](
        logging,
        httpMetrics(metricsOps),
        otel.asHttpRoutesMiddleware,
        errorReporting,
        GZip(_)
      ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
  }

  private object redactor extends PathRedactor with QueryRedactor:
    def redactPath(path: Uri.Path): Uri.Path = path
    def redactQuery(query: Query): Query     =
      if (query.isEmpty) query
      else Query(redact.REDACTED -> None)

}
