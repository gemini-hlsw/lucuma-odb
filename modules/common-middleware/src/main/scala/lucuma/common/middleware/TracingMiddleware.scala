// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import org.http4s.Query
import org.http4s.Uri
import org.http4s.otel4s.middleware.trace.client.UriRedactor
import org.http4s.otel4s.middleware.trace.redact
import org.typelevel.otel4s.Attributes
import org.typelevel.otel4s.trace.Tracer

object TracingMiddleware:

  type Middleware[F[_]] = Endo[HttpRoutes[F]]

  /**
   * A URI redactor for span data providers that redacts the query string (which may carry API keys
   * or other secrets) while leaving the path and fragment intact. Usable for both server and client
   * span data providers.
   */
  val redactor: UriRedactor = new UriRedactor:
    def redactPath(path: Uri.Path): Uri.Path = path
    def redactQuery(query: Query): Query     =
      if query.isEmpty then query
      else Query(redact.REDACTED -> None)
    def redactFragment(fragment: Uri.Fragment): Option[Uri.Fragment] =
      Some(fragment)

  /** A middleware that adds the authenticated user to the current trace span, if known. */
  def traceUser[F[_]: {Monad, Tracer}](
    ssoClient: SsoClient[F, User]
  ): Middleware[F] = routes =>
    Kleisli { req =>
      val putFields: F[Unit] =
        ssoClient.find(req).flatMap {
          case None    => Monad[F].unit
          case Some(u) =>
            Tracer[F].withCurrentSpanOrNoop:
              _.addAttributes(Attributes.from(u))
        }
      OptionT(putFields >> routes(req).value)
    }
