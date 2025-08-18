// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.orcid

import cats.effect.Concurrent
import io.circe.*
import io.circe.syntax.*
import org.http4s.EntityDecoder
import org.http4s.EntityEncoder
import org.http4s.circe.*

case class OrcidPerson(
  name:   OrcidName,
  emails: List[OrcidEmail]
) {

  def primaryEmail: Option[OrcidEmail] =
    emails.find(_.primary)  orElse
    emails.find(_.verified) orElse
    emails.headOption

}

object OrcidPerson {

  implicit val DecoderOrcidPerson: Decoder[OrcidPerson] = c =>
    for {
      n  <- c.downField("name").as[Option[OrcidName]].map(_.getOrElse(OrcidName(None, None, None)))
      es <- c.downField("emails").downField("email").as[List[OrcidEmail]]
    } yield OrcidPerson(n, es)

  implicit def entityDecoderOrcidPerson[F[_]: Concurrent]: EntityDecoder[F, OrcidPerson] =
    jsonOf[F, OrcidPerson]

  implicit val EncoderOrcidPerson: Encoder[OrcidPerson] = p =>
    Json.obj(
      "name"   -> p.name.asJson,
      "emails" -> Json.obj("email" -> p.emails.asJson)
    )

  implicit def entityEncoderOrcidPerson[F[_]]: EntityEncoder[F, OrcidPerson] =
    EntityEncoder[F, Json].contramap(_.asJson)

}