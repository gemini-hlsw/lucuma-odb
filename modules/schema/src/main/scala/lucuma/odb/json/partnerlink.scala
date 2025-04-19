// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.PartnerLinkType
import lucuma.core.model.PartnerLink

trait PartnerLinkCodec {

  given Decoder[PartnerLink.HasPartner] =
    Decoder.instance { c =>
      c.downField("partner").as[Partner].map(PartnerLink.HasPartner.apply)
    }

  given Encoder[PartnerLink.HasPartner] =
    Encoder.instance { a =>
      Json.obj(
        "linkType" -> a.linkType.asJson,
        "partner"  -> a.partner.asJson
      )
    }

  given Decoder[PartnerLink] =
    Decoder.instance { c =>
      c.downField("linkType").as[PartnerLinkType].flatMap:
        case PartnerLinkType.HasPartner            => c.as[PartnerLink.HasPartner]
        case PartnerLinkType.HasNonPartner         => PartnerLink.HasNonPartner.asRight
        case PartnerLinkType.HasUnspecifiedPartner => PartnerLink.HasUnspecifiedPartner.asRight
    }

  given Encoder[PartnerLink] =
    Encoder.instance[PartnerLink] {
      case p: PartnerLink.HasPartner                 => p.asJson
      case _: PartnerLink.HasNonPartner.type         => Json.obj("linkType" -> PartnerLink.HasNonPartner.linkType.asJson)
      case _: PartnerLink.HasUnspecifiedPartner.type => Json.obj("linkType" -> PartnerLink.HasUnspecifiedPartner.linkType.asJson)
    }

}

object partnerlink extends PartnerLinkCodec