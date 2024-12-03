// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.model.OrcidId
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.sso.client.SsoClient
import lucuma.sso.client.SsoGraphQlClient
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

  def ssoGqlClient: SsoGraphQlClient[IO] =
    new SsoGraphQlClient[IO]:

      // The inverse of the User.Id => OrcidId calculation in TestUsers
      def userId(orcidId: OrcidId): User.Id =
        User.Id.fromLong(
          orcidId
            .value
            .init    // trim the checksum
            .replace("-", "")
            .toLong
        ).getOrElse(sys.error(s"Could not covert $orcidId to a User.Id"))

      override def canonicalizePreAuthUser(orcidId: OrcidId): IO[User] =
        validUsers
          .filter:
            case StandardUser(_, _, _, p) => p.orcidId === orcidId
            case _                        => false
          .headOption
          .fold(sys.error(s"Include a standard user with id '${userId(orcidId)}' in the list of 'validUsers' for your test case."))(_.pure[IO])