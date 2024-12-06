// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.sso.client.SsoClient
import org.http4s.*
import org.http4s.headers.Authorization
import org.typelevel.ci.CIString

trait TestSsoClient:

  def validUsers: List[User]

  val Bearer: AuthScheme = CIString("Bearer")

  def authHeader(user: User) = 
    Authorization(Credentials.Token(AuthScheme.Bearer, Gid[User.Id].fromString.reverseGet(user.id)))

  val invalidAuthHeader = Authorization(Credentials.Token(AuthScheme.Bearer, "***"))

  def ssoClient: SsoClient[IO, User] =
    new SsoClient.AbstractSsoClient[IO, User]:
      def find(req: Request[IO]): IO[Option[User]] = OptionT.fromOption[IO](req.headers.get[Authorization]).flatMapF(get).value
      def get(authorization: Authorization): IO[Option[User]] =
        authorization match
          case Authorization(Credentials.Token(Bearer, s)) =>
            Gid[User.Id].fromString.getOption(s).flatMap(id => validUsers.find(_.id === id)).pure[IO]
          case _ => none.pure[IO]