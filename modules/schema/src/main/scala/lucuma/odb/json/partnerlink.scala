// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ExchangePartner
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.model.PartnerLink

trait PartnerLinkCodec:

  given Decoder[PartnerLink.HasGeminiPartner] =
    Decoder.instance: c =>
      c.downField("geminiPartner").as[Partner].map(PartnerLink.HasGeminiPartner.apply)

  given Encoder[PartnerLink.HasGeminiPartner] =
    Encoder.instance: a =>
      Json.obj(
        "linkType"      -> a.linkType.asJson,
        "geminiPartner" -> a.partner.asJson
      )

  given Decoder[PartnerLink.HasExchangePartner] =
    Decoder.instance: c =>
      c.downField("exchangePartner").as[ExchangePartner].map(PartnerLink.HasExchangePartner.apply)

  given Encoder[PartnerLink.HasExchangePartner] =
    Encoder.instance: a =>
      Json.obj(
        "linkType"        -> a.linkType.asJson,
        "exchangePartner" -> a.partner.asJson
      )

  given Decoder[PartnerLink] =
    Decoder.instance: c =>
      c.downField("linkType").as[PartnerLinkType].flatMap:
        case PartnerLinkType.HasGeminiPartner      => c.as[PartnerLink.HasGeminiPartner]
        case PartnerLinkType.HasExchangePartner    => c.as[PartnerLink.HasExchangePartner]
        case PartnerLinkType.HasNonPartner         => PartnerLink.HasNonPartner.asRight
        case PartnerLinkType.HasUnspecifiedPartner => PartnerLink.HasUnspecifiedPartner.asRight

  given Encoder[PartnerLink] =
    Encoder.instance[PartnerLink]:
      case p: PartnerLink.HasGeminiPartner           => p.asJson
      case p: PartnerLink.HasExchangePartner         => p.asJson
      case _: PartnerLink.HasNonPartner.type         => Json.obj("linkType" -> PartnerLink.HasNonPartner.linkType.asJson)
      case _: PartnerLink.HasUnspecifiedPartner.type => Json.obj("linkType" -> PartnerLink.HasUnspecifiedPartner.linkType.asJson)

object partnerlink extends PartnerLinkCodec