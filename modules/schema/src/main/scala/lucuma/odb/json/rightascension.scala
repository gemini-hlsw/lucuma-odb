// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle
import lucuma.core.math.RightAscension

import java.math.RoundingMode.HALF_UP

object rightascension {

  trait DecoderRightAscension {

    given Decoder[RightAscension] =
      Decoder.instance { c =>
        def fromDecimal(field: String, µas: Long): Decoder.Result[Long] =
          c.downField(field).as[BigDecimal].map { bd =>
            (bd * µas).bigDecimal.setScale(0, HALF_UP).longValueExact
          }

        val micro =
          c.downField("microarcseconds")
            .as[Long]
            .orElse(c.downField("microseconds").as[Long].map(_ * 15L))
            .orElse(fromDecimal("degrees", 3_600_000_000L))
            .orElse(fromDecimal("hours", 15 * 3_600_000_000L))

        val hms =
          c.downField("hms")
            .as[String]
            .flatMap(s =>
              RightAscension.fromStringHMS
                .getOption(s)
                .toRight(DecodingFailure(s"Could not parse `$s` as HH:MM:SS", c.history))
            )

        micro.flatMap(l =>
          Angle.microarcseconds.reverse
            .andThen(RightAscension.fromAngleExact)
            .getOption(l)
            .toRight(DecodingFailure("Invalid right ascension", c.history))
        ).orElse(hms)
      }
  }

  object decoder extends DecoderRightAscension

  trait QueryCodec extends DecoderRightAscension {

    given Encoder_Right_Ascension: Encoder[RightAscension] =
      Encoder.instance { ra =>
        val ha  = ra.toHourAngle
        val a   = ra.toAngle
        val µas = a.toMicroarcseconds
        val µs  = ha.toMicroseconds

        def divMicro(micro:    Long, denom: Int): Json = (BigDecimal(micro, 6) / denom).asJson
        def divArcseconds(div: Int): Json = divMicro(µas, div)
        def divSeconds(div:    Int): Json = divMicro(µs, div)

        Json.obj(
          "hms"             -> HourAngle.HMS(ha).format.asJson,
          "hours"           -> divSeconds(3_600),
          "degrees"         -> divArcseconds(3_600),
          "microarcseconds" -> µas.asJson,
          "microseconds"    -> µs.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderRightAscension {

    given Encoder_Right_Ascension: Encoder[RightAscension] =
      Encoder.instance { ra =>
        Json.obj(
          "microseconds" -> ra.toHourAngle.toMicroseconds.asJson
        )
      }
  }

  object transport extends TransportCodec
}
