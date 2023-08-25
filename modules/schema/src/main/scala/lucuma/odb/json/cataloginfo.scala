// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Encoder
import io.circe.Json
import io.circe.refined.given
import io.circe.syntax.*
import lucuma.core.model.CatalogInfo

object cataloginfo {

  trait QueryEncoder {

    given Encoder[CatalogInfo] =
      Encoder.instance { ci =>
        Json.obj(
          "name" -> ci.catalog.asJson,
          "id" -> ci.id.asJson,
          "objectType" -> ci.objectType.asJson
        )
      }
  }

  object query extends QueryEncoder
}
