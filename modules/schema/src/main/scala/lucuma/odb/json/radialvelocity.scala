// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.RadialVelocity

object radialvelocity {

  trait DecoderRadialVelocity {

    given Decoder[RadialVelocity] =
      Decoder.instance { c =>
        val bd =
          c.downField("metersPerSecond")
            .as[BigDecimal]
            .orElse(c.downField("kilometersPerSecond").as[BigDecimal].map(_ * BigDecimal(1000)))
            .orElse(c.downField("centimetersPerSecond").as[BigDecimal].map(_ / BigDecimal(100)))

        bd.flatMap { v =>
          RadialVelocity.fromMetersPerSecond.getOption(v).toRight(DecodingFailure("Invalid radial velocity", c.history))
        }.orElse(DecodingFailure("Could not parse radialvelocity", c.history).asLeft)
      }
  }

  object decoder extends DecoderRadialVelocity

  trait QueryCodec extends DecoderRadialVelocity {

    given Encoder_Radial_Velocity: Encoder[RadialVelocity] =
      Encoder.instance { rv =>
        Json.obj(
          "metersPerSecond"      -> rv.rv.value.asJson,
          "kilometersPerSecond"  -> (rv.rv.value / BigDecimal(1000)).asJson,
          "centimetersPerSecond" -> (rv.rv.value * BigDecimal(100)).toLong.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderRadialVelocity {

    given Encoder_Radial_Velocity: Encoder[RadialVelocity] =
      Encoder.instance { rv =>
        Json.obj(
          "metersPerSecond"      -> rv.rv.value.asJson,
        )
      }
  }

  object transport extends TransportCodec
}
