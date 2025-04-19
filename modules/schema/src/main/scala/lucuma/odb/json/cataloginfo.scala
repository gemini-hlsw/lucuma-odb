// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.core.enums.CatalogName
import lucuma.core.model.CatalogInfo

trait CatalogInfoCodec {

  given Decoder[CatalogInfo] =
    Decoder.instance(c =>
      for {
        name <- c.downField("name").as[CatalogName]
        id   <- c.downField("id").as[NonEmptyString]
        ot   <- c.downField("objectType").as[Option[NonEmptyString]]
      } yield CatalogInfo(name, id, ot)
    )

  given Encoder[CatalogInfo] =
    Encoder.instance { ci =>
      Json.obj(
        "name"       -> ci.catalog.asJson,
        "id"         -> ci.id.asJson,
        "objectType" -> ci.objectType.asJson
      )
    }
}

object cataloginfo extends CatalogInfoCodec
