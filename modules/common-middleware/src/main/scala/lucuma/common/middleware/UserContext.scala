// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.Monad
import cats.data.OptionT
import cats.syntax.all.*
import grackle.Env
import lucuma.common.middleware.UserAttributes.given
import lucuma.core.model.User
import lucuma.graphql.routes.Auth
import lucuma.graphql.routes.Authenticator
import lucuma.sso.client.SsoClient
import org.typelevel.otel4s.Attributes

/** How the GraphQL routes of each service supply the authenticated user to its Grackle mapping. */
object UserContext:

  /** The key under which the routes supply the authenticated user in the Grackle `Env`. */
  val EnvKey: String = "user"

  /** A Grackle `Env` that holds `user`. */
  def env(user: User): Env =
    Env(EnvKey -> user)

  val AccessDenied: String = "Access denied."

  /** Authenticates each request with `client`. */
  def authenticator[F[_]: Monad, U <: User](client: SsoClient[F, U]): Authenticator[F] =
    authenticator(client, _ => Monad[F].unit)

  /** Authenticates each request with `client`, and runs `onUser` for each user it resolves. */
  def authenticator[F[_]: Monad, U <: User](client: SsoClient[F, U], onUser: U => F[Unit]): Authenticator[F] =
    Authenticator:
      case None    => Auth.Anonymous.pure[F]
      case Some(a) =>
        OptionT(client.get(a))
          .semiflatTap(onUser)
          .fold(Auth.Denied(AccessDenied))(u => Auth(env(u), Attributes.from(u: User)))
