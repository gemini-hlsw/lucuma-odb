// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Igrins2OffsetMode
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig
import lucuma.core.model.sequence.igrins2.Igrins2SVCImages
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig
import lucuma.core.util.TimeSpan

trait Igrins2Codec:

  import time.decoder.given

  given Decoder[Igrins2StaticConfig] =
    Decoder.instance: c =>
      for
        s <- c.downField("saveSVCImages").as[Boolean]
        o <- c.downField("offsetMode").as[Igrins2OffsetMode]
      yield Igrins2StaticConfig(Igrins2SVCImages(s), o)

  given Encoder[Igrins2StaticConfig] =
    Encoder.instance: a =>
      Json.obj(
        "saveSVCImages" -> a.saveSVCImages.value.asJson,
        "offsetMode"    -> a.offsetMode.asJson
      )

  given Decoder[Igrins2DynamicConfig] =
    Decoder.instance:
      _.downField("exposure").as[TimeSpan].map(Igrins2DynamicConfig.apply)

  given (using Encoder[TimeSpan]): Encoder[Igrins2DynamicConfig] =
    Encoder.instance: a =>
      Json.obj("exposure" -> a.exposure.asJson)

object igrins2 extends Igrins2Codec
