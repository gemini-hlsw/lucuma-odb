// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.syntax.*
import lucuma.core.math.Coordinates
import io.circe.Json

object coordinates:

  trait QueryCodec:
    import rightascension.query.given
    import declination.query.given

    given Encoder_Coordinates: Encoder[Coordinates] = cs =>
      Json.obj(
        "ra"  -> cs.ra.asJson,
        "dec" -> cs.dec.asJson
      )

    given Decoder_Coordinates: Decoder[Coordinates] = ???

  object query extends QueryCodec

