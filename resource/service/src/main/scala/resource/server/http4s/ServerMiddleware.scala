// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server.http4s
import cats.*
import cats.effect.*
import fs2.compression.Compression
import lucuma.common.middleware.CorsMiddleware
import lucuma.common.middleware.ErrorReportingMiddleware
import lucuma.common.middleware.LoggingMiddleware
import lucuma.common.middleware.TracingMiddleware
import lucuma.core.model.User
import lucuma.otel.HttpInstrumentation
import lucuma.sso.client.SsoClient
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.server.middleware.GZip
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.Tracer
import org.typelevel.otel4s.trace.TracerProvider

object ServerMiddleware {
  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** HTTP metrics and spans. Apply to the final app, after `orNotFound`. */
  def instrumentation[F[_]: {Async, TracerProvider, MeterProvider, ContextManager}]
    : F[Endo[HttpApp[F]]] =
    HttpInstrumentation.server[F]()

  /**
   * Create a combined middleware to all server routes, except instrumentation
   */
  def apply[F[_]: {Async, Compression, Tracer}](
    corsOverHttps: Boolean,
    domain:        Seq[String],
    ssoClient:     SsoClient[F, User]
  ): Middleware[F] = {
    given Logger[F] = Slf4jLogger.getLogger[F]

    List[Middleware[F]](
      CorsMiddleware.cors[F](corsOverHttps, domain.toList),
      LoggingMiddleware.logging[F](),
      TracingMiddleware.traceUser(ssoClient),
      ErrorReportingMiddleware.errorReporting,
      GZip(_)
    ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
  }

}
