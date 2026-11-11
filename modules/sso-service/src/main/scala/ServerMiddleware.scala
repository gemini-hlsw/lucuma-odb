// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.*
import cats.effect.*
import lucuma.common.middleware.CorsMiddleware
import lucuma.common.middleware.LoggingMiddleware
import lucuma.otel.HttpInstrumentation
import lucuma.sso.service.config.Config
import lucuma.sso.service.config.Environment
import lucuma.sso.service.config.Environment.*
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.server.middleware.ErrorAction
import org.typelevel.log4cats.Logger
import org.typelevel.otel4s.context.ContextManager
import org.typelevel.otel4s.metrics.MeterProvider
import org.typelevel.otel4s.trace.TracerProvider

/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** Adds HTTP metrics and distributed tracing via OpenTelemetry. Apply to the final app, after `orNotFound`. */
  def instrumentation[F[_]: Async: TracerProvider: MeterProvider: ContextManager]: F[Endo[HttpApp[F]]] =
    HttpInstrumentation.server[F](recordHeaders = true)

  /** A middleware that logs request and response. Sensitive headers are redacted outside Local. */
  def logging[F[_]: Async](
    env:          Environment,
  ): Middleware[F] =
    LoggingMiddleware.logging[F](revealSensitiveHeaders =
      env match
        case Local                         => true
        case Review | Staging | Production => false
    )

  /** A middleware that reports errors during requets processing. */
  def errorReporting[F[_]: MonadThrow: Logger]: Middleware[F] = routes =>
    ErrorAction.httpRoutes.log(
      httpRoutes              = routes,
      messageFailureLogAction = Logger[F].error(_)(_),
      serviceErrorLogAction   = Logger[F].error(_)(_)
    )

  /** A middleware that composes all the others defined in this module, except instrumentation. */
  def apply[F[_]: Async: Logger](
    config: Config,
  ): Middleware[F] =
    List[Middleware[F]](
      CorsMiddleware.cors(domain = List(config.cookieDomain)),
      logging(config.environment),
      errorReporting,
    ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`
