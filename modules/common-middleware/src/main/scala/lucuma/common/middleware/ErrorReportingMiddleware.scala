// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.*
import org.http4s.HttpRoutes
import org.http4s.server.middleware.ErrorAction
import org.typelevel.log4cats.Logger

object ErrorReportingMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** A middleware that reports errors during request processing. */
  def errorReporting[F[_]: MonadThrow: Logger]: Middleware[F] = routes =>
    ErrorAction.httpRoutes.log(
      httpRoutes = routes,
      messageFailureLogAction = Logger[F].error(_)(_),
      serviceErrorLogAction = Logger[F].error(_)(_)
    )
