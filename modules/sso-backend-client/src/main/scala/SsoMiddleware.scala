// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import lucuma.core.model.User
import natchez.Trace
import org.http4s.HttpRoutes

/**
 * A middleware that adds the following Lucuma-standard fields to the current span:
 *
 * - "lucuma.user"      -> User's display name.
 * - "lucuma.user.id    -> User id, like "u-2e4a"
 * - "lucuma.user.role" -> User role, like "PI", "Guest", "NGO(Chile)"
 */
object SsoMiddleware {

  // The SSO service itself can't use this middleware but it does need to log the user, so we're
  // exposing it to the `sso` package.
  private[sso] def traceUser[F[_]: Trace](u: User, prefix: String = "lucuma"): F[Unit] =
    Trace[F].put(
      s"$prefix.user"      -> u.displayName,
      s"$prefix.user.id"   -> u.id.toString,
      s"$prefix.user.role" -> u.role.name,
    )

  def apply[F[_]: MonadCancelThrow: Trace](ssoClient: SsoClient[F, User])(routes: HttpRoutes[F]): HttpRoutes[F] =
    Kleisli { req =>
      for {
        ou  <- OptionT.liftF(ssoClient.find(req))
        _   <- ou.traverse(traceUser[OptionT[F, *]](_))
        res <- routes.run(req)
      } yield res
    }

}