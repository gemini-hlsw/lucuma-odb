// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.odb.data.PartnerAssociation

class partner {

  given Encoder[PartnerAssociation] =
    Encoder.instance { a =>
      Json.fromFields(
        ("isSet" -> a.isSet.asJson) :: (a match {
          case PartnerAssociation.HasPartner(p) => List("partner" -> p.asJson)
          case _ => Nil
        })
      )
    }

  given Decoder[PartnerAssociation] =
    Decoder.instance { c =>
      for {
        s <- c.downField("isSet").as[Boolean]
        p <- c.downField("partner").as[Option[Partner]]
      } yield PartnerAssociation.fromFields(s, p)
    }

}