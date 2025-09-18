// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.sequence.gmos.GmosCcdMode

// Internalized from lucuma-odb
trait GmosCodec {

  given Decoder[GmosCcdMode] =
    Decoder.instance { c =>
      for {
        x <- c.downField("xBin").as[GmosXBinning]
        y <- c.downField("yBin").as[GmosYBinning]
        n <- c.downField("ampCount").as[GmosAmpCount]
        g <- c.downField("ampGain").as[GmosAmpGain]
        m <- c.downField("ampReadMode").as[GmosAmpReadMode]
      } yield GmosCcdMode(x, y, n, g, m)
    }

  given Encoder[GmosCcdMode] =
    Encoder.instance { (a: GmosCcdMode) =>
      Json.obj(
        "xBin"        -> a.xBin.asJson,
        "yBin"        -> a.yBin.asJson,
        "ampCount"    -> a.ampCount.asJson,
        "ampGain"     -> a.ampGain.asJson,
        "ampReadMode" -> a.ampReadMode.asJson
      )
    }

}

object gmos extends GmosCodec
