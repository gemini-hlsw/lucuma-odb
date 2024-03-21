// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.HourAngle

import java.math.RoundingMode.HALF_UP

object angle {

  trait DecoderAngle {

    given Decoder[Angle] =
      Decoder.instance { c =>
        def fromDecimal(field: String, µas: Long): Decoder.Result[Long] =
          c.downField(field).as[BigDecimal].map { bd =>
            (bd * µas).bigDecimal.setScale(0, HALF_UP).longValueExact
          }

        val micro =
          c.downField("microarcseconds").as[Long]   orElse
          fromDecimal("microseconds",          15L) orElse
          fromDecimal("milliarcseconds",    1_000L) orElse
          fromDecimal("milliseconds",      15_000L) orElse
          fromDecimal("arcseconds",     1_000_000L) orElse
          fromDecimal("seconds",       15_000_000L) orElse
          fromDecimal("arcminutes",    60_000_000L) orElse
          fromDecimal("minutes",  15 * 60_000_000L) orElse
          fromDecimal("degrees",    3_600_000_000L) orElse
          fromDecimal("hours", 15 * 3_600_000_000L)

        def dms: Decoder.Result[Angle] =
          c.downField("dms")
           .as[String]
           .flatMap { s =>
             Angle
               .fromStringSignedDMS
               .getOption(s)
               .toRight(DecodingFailure(s"Could not parse `$s` as DD:MM:SS", c.history))
           }

        def hms: Decoder.Result[Angle] =
          c.downField("hms")
           .as[String]
           .flatMap { s =>
             HourAngle
               .fromStringHMS
               .getOption(s)
               .toRight(DecodingFailure(s"Could not parse `$s` as HH:MM:SS", c.history))
               .map(HourAngle.angle.get)
           }

        micro.map(Angle.fromMicroarcseconds) orElse dms orElse hms orElse
          DecodingFailure(s"Could not parse angle", c.history).asLeft
      }

  }

  object decoder extends DecoderAngle

  trait QueryCodec extends DecoderAngle {

    given Encoder_Angle: Encoder[Angle] =
      Encoder.instance { a =>
        val ha  = Angle.hourAngle.get(a)
        val µas = a.toMicroarcseconds
        val µs  = ha.toMicroseconds

        def divMicro(micro: Long, denom: Int): Json = (BigDecimal(micro, 6) / denom).asJson
        def divArcseconds(div: Int): Json           = divMicro(µas, div)
        def divSeconds(div: Int): Json              = divMicro(µs,  div)

        Json.obj(
          "microarcseconds" -> µas.asJson,
          "microseconds"    -> µs.asJson,
          "milliarcseconds" -> BigDecimal(µas, 3).asJson,
          "milliseconds"    -> BigDecimal(µs,  3).asJson,
          "arcseconds"      -> BigDecimal(µas, 6).asJson,
          "seconds"         -> BigDecimal(µs,  6).asJson,
          "arcminutes"      -> divArcseconds(60),
          "minutes"         -> divSeconds(60),
          "degrees"         -> divArcseconds(3_600),
          "hours"           -> divSeconds(3_600),
          "dms"             -> Angle.dms.get(a).format.asJson,
          "hms"             -> HourAngle.HMS(ha).format.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderAngle {
    given Encoder_Angle: Encoder[Angle] =
      Encoder.instance { a =>
        Json.obj(
          "microarcseconds" -> a.toMicroarcseconds.asJson
        )
      }
  }

  object transport extends TransportCodec
}

