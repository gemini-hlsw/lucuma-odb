// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.effect.*
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.sso.client.codec.user.EncoderUser
import org.http4s.*
import org.http4s.Method.POST
import org.http4s.circe.jsonEncoderOf
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl

trait OdbClient[F[_]]:

  /** Transfer ownership of a guests's program to a standard user, acting on behalf of the SSO service user. */
  def transferOwnership(from: GuestUser, to: StandardUser): F[Unit]

  protected def chownRequest(
    from: GuestUser,
    to: StandardUser,
    ssoJwtWriter: SsoJwtWriter[F],
    odbRootUri: Uri,
    serviceUser: ServiceUser
  ): F[Request[F]] = 
    val dsl = Http4sClientDsl[F]; import dsl.*
    given EntityEncoder[F, Json] = jsonEncoderOf
    ssoJwtWriter.addAuthorizationHeader(
      serviceUser,
      POST(odbRootUri / "chown")
        .withEntity:
          Json.obj(
            "from" -> EncoderUser(from), 
            "to" -> EncoderUser(to)
          )
    )

object OdbClient:

  def apply[F[_]: Concurrent](
    client: Client[F],
    ssoJwtWriter: SsoJwtWriter[F],
    odbRootUri: Uri,
    serviceUser: ServiceUser
  ): OdbClient[F] = 
    new OdbClient[F[_]]:    
      def transferOwnership(from: GuestUser, to: StandardUser): F[Unit] =
        chownRequest(from, to, ssoJwtWriter, odbRootUri, serviceUser).flatMap: req =>
          client.run(req).use: res =>
            if res.status.isSuccess then MonadCancelThrow[F].unit
            else 
              res.as[String].flatMap: msg =>
                MonadCancelThrow[F].raiseError(new RuntimeException(msg))


