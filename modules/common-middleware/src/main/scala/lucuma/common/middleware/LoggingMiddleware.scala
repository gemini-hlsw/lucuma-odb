// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.*
import cats.effect.Async
import org.http4s.Headers
import org.http4s.HttpRoutes
import org.http4s.server.middleware.Logger

object LoggingMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * A middleware that logs requests and responses.
   *
   * Bodies are never logged, since they may carry secrets (tokens, client secrets).
   * Sensitive headers (Authorization, Cookie, ...) are redacted unless `revealSensitiveHeaders`
   * is true, which should only be enabled in local development.
   */
  def logging[F[_]: Async](revealSensitiveHeaders: Boolean = false): Middleware[F] =
    Logger.httpRoutes[F](
      logHeaders        = true,
      logBody           = false,
      redactHeadersWhen = h => !revealSensitiveHeaders && Headers.SensitiveHeaders.contains(h)
    )
