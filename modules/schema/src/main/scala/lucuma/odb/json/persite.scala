// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.PerSite

object persite:

  given [A: Encoder]: Encoder[PerSite[A]] = ps =>
    Json.obj(
      "gn" -> ps.gn.asJson,
      "gs" -> ps.gs.asJson
    )

  given [A: Decoder]: Decoder[PerSite[A]] = hc =>
    for 
      gn <- hc.downField("gn").as[A]
      gs <- hc.downField("gs").as[A]
    yield PerSite(gn, gs)

