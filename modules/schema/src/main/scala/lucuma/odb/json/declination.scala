// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Declination

import java.math.RoundingMode.HALF_UP

object declination {
  trait DecoderDeclination {
    given Decoder[Declination] =
      Decoder.instance { c =>
        def fromDecimal(field: String, µas: Long): Decoder.Result[Long] =
          c.downField(field).as[BigDecimal].map { bd =>
            (bd * µas).bigDecimal.setScale(0, HALF_UP).longValueExact
          }

        val micro: Decoder.Result[Declination] =
          c.downField("microarcseconds")
            .as[Long]
            .orElse(fromDecimal("degrees", Angle.µasPerDegree))
            .flatMap { l =>
              Declination.fromAngle
                .getOption(Angle.fromMicroarcseconds(l))
                .toRight(DecodingFailure("Invalid µarcsec value for declination", c.history))
            }

        def dms: Decoder.Result[Declination] =
          c.downField("dms")
            .as[String]
            .flatMap { s =>
              Declination.fromStringSignedDMS
                .getOption(s)
                .toRight(DecodingFailure(s"Could not parse `$s` as DD:MM:SS", c.history))
            }

        micro
          .orElse(dms)
          .orElse(DecodingFailure("Could not parse declination", c.history).asLeft)
      }
  }

  object decoder extends DecoderDeclination

  trait QueryCodec extends DecoderDeclination {

    given Encoder_Declination: Encoder[Declination] =
      Encoder.instance { dec =>
        Json.obj(
          "dms"             -> Declination.fromStringSignedDMS.reverseGet(dec).asJson,
          "degrees"         -> BigDecimal(dec.toAngle.toDoubleDegrees).asJson,
          "microarcseconds" -> dec.toAngle.toMicroarcseconds.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderDeclination {

    given Encoder_Declination: Encoder[Declination] =
      Encoder.instance { dec =>
        Json.obj(
          "microarcseconds" -> dec.toAngle.toMicroarcseconds.asJson
        )
      }
  }

  object transport extends TransportCodec
}
