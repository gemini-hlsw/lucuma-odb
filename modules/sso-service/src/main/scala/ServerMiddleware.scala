// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.*
import cats.effect.*
import lucuma.common.middleware.CorsMiddleware
import lucuma.common.middleware.LoggingMiddleware
import lucuma.sso.service.config.Config
import lucuma.sso.service.config.Environment
import lucuma.sso.service.config.Environment.*
import natchez.Trace
import natchez.http4s.NatchezMiddleware
import org.http4s.HttpRoutes
import org.http4s.server.middleware.ErrorAction
import org.typelevel.log4cats.Logger

/** A module of all the middlewares we apply to the server routes. */
object ServerMiddleware {

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** A middleware that adds distributed tracing. */
  def natchez[F[_]: Trace](implicit ev: MonadCancel[F, Throwable]): Middleware[F] =
    NatchezMiddleware.server[F]

  /** A middleware that logs request and response. Sensitive headers are redacted outside Local. */
  def logging[F[_]: Async](
    env:          Environment,
  ): Middleware[F] =
    LoggingMiddleware.logging[F](revealSensitiveHeaders = env match {
      case Local                         => true
      case Review | Staging | Production => false
    })

  /** A middleware that reports errors during requets processing. */
  def errorReporting[F[_]: MonadThrow: Logger]: Middleware[F] = routes =>
    ErrorAction.httpRoutes.log(
      httpRoutes              = routes,
      messageFailureLogAction = Logger[F].error(_)(_),
      serviceErrorLogAction   = Logger[F].error(_)(_)
    )

  /** A middleware that composes all the others defined in this module. */
  def apply[F[_]: Async: Trace: Logger](
    config: Config,
  ): Middleware[F] =
    List[Middleware[F]](
      CorsMiddleware.cors(domain = List(config.cookieDomain)),
      logging(config.environment),
      natchez,
      errorReporting,
    ).reduce(_ andThen _) // N.B. the monoid for Endo uses `compose`

}
