// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.data.OptionT
import cats.effect.*
import cats.syntax.all.*
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.sso.client.ApiKey
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoClient.AbstractSsoClient
import lucuma.sso.client.SsoJwtReader
import lucuma.sso.service.database.Database
import org.http4s.Credentials.Token
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString

/**
 * Constructor for an SsoClient that runs locally, which we need for endpoints that are going to be
 * authenticated normally.
 */
object LocalSsoClient {

  def apply[F[_]: Sync](
    jwtReader: SsoJwtReader[F],
    dbPool:    Resource[F, Database[F]]
  ): SsoClient[F, User] =
    new AbstractSsoClient[F, User] {

      val Bearer = CIString("Bearer")

      def fetchApiUser(apiKey: ApiKey): F[Option[StandardUser]] =
        dbPool.use(_.findStandardUserFromApiKey(apiKey))

      def getApiKey(bearerAuthorization: String): Option[ApiKey] =
        ApiKey.fromString.getOption(bearerAuthorization)

      def getApiUser(bearerAuthorization: String): F[Option[StandardUser]] =
        getApiKey(bearerAuthorization).flatTraverse(fetchApiUser)

      def getJwtInfo(bearerAuthorization: String): F[Either[String, User]] =
        jwtReader.decodeUser(bearerAuthorization).attempt.map(_.leftMap(SsoClient.denialMessage))

      def getUserInfo(bearerAuthorization: String): F[Either[String, User]] =
        OptionT(getApiUser(bearerAuthorization).widen[Option[User]])
          .map(_.asRight[String])
          .getOrElseF(getJwtInfo(bearerAuthorization))

      def get(authorization: Authorization): F[Either[String, User]] =
        authorization.credentials match {
          case Token(Bearer, ba) => getUserInfo(ba)
          case _                 => SsoClient.AccessDenied.asLeft[User].pure[F]
        }

    }

}
