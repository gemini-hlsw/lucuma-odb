// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.OffsetPurpose
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.sequence.TelescopeConfigSpec

object telescopeConfigSpec {

  // An absent `explicitPurpose` is an unspecified one, so patterns stored
  // before purposes existed decode unchanged.
  trait DecoderTelescopeConfigSpec {
    import offset.decoder.given

    given Decoder[TelescopeConfigSpec] =
      Decoder.instance: c =>
        for
          o <- c.downField("offset").as[Offset]
          g <- c.downField("guiding").as[StepGuideState]
          p <- c.downField("explicitPurpose").as[Option[OffsetPurpose]]
        yield TelescopeConfigSpec(o, g, p)
  }

  object decoder extends DecoderTelescopeConfigSpec

  // GraphQL results: `explicitPurpose` is null when unspecified.
  trait QueryCodec extends DecoderTelescopeConfigSpec {
    import offset.query.given

    given Encoder[TelescopeConfigSpec] =
      Encoder.instance: a =>
        Json.obj(
          "offset"          -> a.offset.asJson,
          "guiding"         -> a.guiding.asJson,
          "explicitPurpose" -> a.explicitPurpose.asJson
        )
  }

  object query extends QueryCodec

  // Stored form: `explicitPurpose` is omitted when unspecified, so patterns
  // without one serialize exactly as they did before purposes existed.
  trait TransportCodec extends DecoderTelescopeConfigSpec {
    import offset.transport.given

    given Encoder[TelescopeConfigSpec] =
      Encoder.instance: a =>
        Json.obj(
          "offset"          -> a.offset.asJson,
          "guiding"         -> a.guiding.asJson,
          "explicitPurpose" -> a.explicitPurpose.asJson
        ).dropNullValues
  }

  object transport extends TransportCodec
}
