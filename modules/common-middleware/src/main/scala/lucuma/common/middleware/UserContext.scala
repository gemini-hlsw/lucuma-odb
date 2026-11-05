// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.common.middleware

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import grackle.Env
import lucuma.common.middleware.UserAttributes.given
import lucuma.core.model.User
import lucuma.graphql.routes.Auth
import lucuma.graphql.routes.Authenticator
import lucuma.graphql.routes.RequestContext
import lucuma.sso.client.SsoClient
import org.typelevel.otel4s.Attributes

/** How the GraphQL routes of each service supply the authenticated user to its Grackle mapping. */
object UserContext:

  /** The key under which the routes supply the authenticated user in the Grackle `Env`. */
  val EnvKey: String = "user"

  /** A Grackle `Env` that holds `user`. */
  def env(user: User): Env =
    Env(EnvKey -> user)

  /** Authenticates each request with `client`. */
  def authenticator[F[_]: Monad, U <: User](client: SsoClient[F, U]): Authenticator[F] =
    authenticator(client, _ => Monad[F].unit)

  /** Authenticates each request with `client`, and runs `onUser` for each user it resolves. */
  def authenticator[F[_]: Monad, U <: User](client: SsoClient[F, U], onUser: U => F[Unit]): Authenticator[F] =
    authenticator(client, onUser, u => RequestContext(env(u), Attributes.from(u: User)))

  /**
   * Authenticates each request with `client`, which may resolve more than a user, runs `onUser`
   * for each value it resolves, and builds the request context from that value with `context`.
   */
  def authenticator[F[_]: Monad, A](
    client:  SsoClient[F, A],
    onUser:  A => F[Unit],
    context: A => RequestContext
  ): Authenticator[F] =
    Authenticator:
      case None    => Auth.Anonymous.pure[F]
      case Some(a) =>
        EitherT(client.get(a))
          .semiflatTap(onUser)
          .fold(Auth.Denied(_), a => Auth.Authenticated(context(a)))
