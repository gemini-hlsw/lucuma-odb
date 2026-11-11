// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.*
import cats.data.Kleisli
import cats.data.OptionT
import cats.syntax.all.*
import lucuma.common.middleware.UserAttributes.given
import lucuma.core.model.User
import lucuma.sso.client.SsoClient
import org.http4s.HttpRoutes
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.trace.Tracer

object TracingMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /** A middleware that adds the authenticated user to the current trace span, if known. */
  def traceUser[F[_]: {Monad, Tracer}](
    ssoClient: SsoClient[F, User]
  ): Middleware[F] = routes =>
    Kleisli { req =>
      val putFields: F[Unit] =
        ssoClient.find(req).flatMap(_.traverse_ (u =>
          Tracer[F].withCurrentSpanOrNoop(_.addAttributes(Attributes.from(u)))
        ))
      OptionT(putFields >> routes(req).value)
    }
