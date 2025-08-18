// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.orcid

import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.syntax.*
import lucuma.core.model.OrcidId
import lucuma.core.model.OrcidProfile
import lucuma.core.model.UserProfile

case class OrcidName(
  familyName: Option[String],
  givenName:  Option[String],
  creditName: Option[String],
):
  def displayName(orcidId: OrcidId): String =
    OrcidProfile(orcidId, UserProfile(givenName, familyName, creditName, None)).displayName

object OrcidName:

  case class Value(value: String) // annoying
  object Value {
    implicit val EncoderValue: Encoder[Value] = deriveEncoder
    implicit val DecoderValue: Decoder[Value] = deriveDecoder
  }

  implicit val DecoderOrcidName: Decoder[OrcidName] = c =>
    for {
      f <- c.downField("family-name").as[Option[Value]]
      g <- c.downField("given-names").as[Option[Value]]
      c <- c.downField("credit-name").as[Option[Value]]
    } yield OrcidName(f.map(_.value),g.map(_.value),c.map(_.value))

  implicit val EncoderOrcidName: Encoder[OrcidName] = n =>
    Json.obj(
      "family-name" -> n.familyName.map(Value(_)).asJson,
      "given-names" -> n.givenName.map(Value(_)).asJson,
      "credit-name" -> n.creditName.map(Value(_)).asJson,
    )