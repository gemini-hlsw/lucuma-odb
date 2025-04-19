// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Epoch

trait EpochCodec {
  
  given Codec[Epoch] with {
    def apply(e: Epoch): Json =
      Epoch.fromString.reverseGet(e).asJson

    def apply(c: HCursor): Decoder.Result[Epoch] = 
      Decoder[String].apply(c).flatMap {s =>
        Epoch.fromString.getOption(s).toRight(DecodingFailure(s"Invalid epoch value: $s", c.history))
        }
  }
}

object epoch extends EpochCodec
