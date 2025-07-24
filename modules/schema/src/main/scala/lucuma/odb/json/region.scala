// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angular
import lucuma.core.math.Arc
import lucuma.core.math.Declination
import lucuma.core.math.Region
import lucuma.core.math.RightAscension

object region {
  import arc.given

  object decoder:
    import rightascension.decoder.given
    import declination.decoder.given
    given Decoder[Region] = hc =>
      for 
        ra <-  hc.downField("rightAscensionArc").as[Arc[RightAscension]]
        dec <- hc.downField("declinationArc").as[Arc[Declination]]
      yield Region(ra, dec)

  private def encoder(using Encoder[RightAscension], Encoder[Declination]): Encoder[Region] = a =>
    Json.obj(
      "rightAscensionArc" -> a.raArc.asJson,
      "declinationArc" -> a.decArc.asJson
    )

  object query:
    import rightascension.query.given
    import declination.query.given
    export decoder.given
    given Encoder[Region] = encoder

  object transport:
    import rightascension.transport.given
    import declination.transport.given
    export decoder.given
    given Encoder[Region] = encoder
  
}

