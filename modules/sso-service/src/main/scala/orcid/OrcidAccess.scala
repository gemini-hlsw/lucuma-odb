// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.orcid

import cats.effect.Concurrent
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.OrcidId
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.circe.*

import java.time.Duration
import java.util.UUID

final case class OrcidAccess(
  accessToken:  UUID,
  tokenType:    String,
  refreshToken: UUID,
  expiresIn:    Duration,
  scope:        String,
  name:         String,
  orcidId:      OrcidId
)

object OrcidAccess {

  implicit val DecoderOrcidId: Decoder[OrcidId] =
    Decoder[String].emap(OrcidId.fromValue)

  implicit val DecoderOrcidAccess: Decoder[OrcidAccess] = c =>
    for {
      a <- c.downField("access_token").as[UUID]
      t <- c.downField("token_type").as[String]
      r <- c.downField("refresh_token").as[UUID]
      e <- c.downField("expires_in").as[Long].map(Duration.ofSeconds)
      s <- c.downField("scope").as[String]
      n <- c.downField("name").as[String]
      o <- c.downField("orcid").as[OrcidId]
    } yield OrcidAccess(a, t, r, e, s, n, o)

  implicit def entityDecoderOrcidAccess[F[_]: Concurrent]: EntityDecoder[F, OrcidAccess] =
    jsonOf[F, OrcidAccess]

  implicit val EncoderOrcidAccess: Encoder[OrcidAccess] = a =>
    Json.obj(
      ("access_token"  -> a.accessToken.asJson),
      ("token_type"    -> a.tokenType.asJson),
      ("refresh_token" -> a.refreshToken.asJson),
      ("expires_in"    -> a.expiresIn.getSeconds.asJson),
      ("scope"         -> a.scope.asJson),
      ("name"          -> a.name.asJson),
      ("orcid"         -> a.orcidId.asJson),
    )

  implicit def entityEncoderOrcidAccess[F[_]]: EntityEncoder[F, OrcidAccess] =
    EntityEncoder[F, Json].contramap(_.asJson)

}