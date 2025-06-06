// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension

object coordinates:

  trait QueryCodec:
    import rightascension.query.given
    import declination.query.given

    given Encoder_Coordinates: Encoder[Coordinates] = cs =>
      Json.obj(
        "ra"  -> cs.ra.asJson,
        "dec" -> cs.dec.asJson
      )

    given Decoder_Coordinates: Decoder[Coordinates] = hc =>
      for 
        r <- hc.downField("ra").as[RightAscension]
        d <- hc.downField("dec").as[Declination]
      yield Coordinates(r, d)

  object query extends QueryCodec

