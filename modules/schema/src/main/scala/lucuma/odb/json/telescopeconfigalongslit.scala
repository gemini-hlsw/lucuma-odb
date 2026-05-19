// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfigAlongSlit

object telescopeConfigAlongSlit {

  trait DecoderTelescopeConfigAlongSlit {
    import offset.decoder.given

    given Decoder[TelescopeConfigAlongSlit] =
      Decoder.instance: c =>
        for
          q <- c.downField("q").as[Offset.Q]
          g <- c.downField("guiding").as[StepGuideState]
        yield TelescopeConfigAlongSlit(q, g)
  }

  object decoder extends DecoderTelescopeConfigAlongSlit

  trait QueryCodec extends DecoderTelescopeConfigAlongSlit {
    import offset.query.given

    given Encoder[TelescopeConfigAlongSlit] =
      Encoder.instance: a =>
        Json.obj(
          "q"       -> a.offset.asJson,
          "guiding" -> a.guiding.asJson
        )
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderTelescopeConfigAlongSlit {
    import offset.transport.given

    given Encoder[TelescopeConfigAlongSlit] =
      Encoder.instance: a =>
        Json.obj(
          "q"       -> a.offset.asJson,
          "guiding" -> a.guiding.asJson
        )
  }

  object transport extends TransportCodec
}
