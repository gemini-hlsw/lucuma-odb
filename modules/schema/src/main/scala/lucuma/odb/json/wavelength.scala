// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.math.Wavelength
import lucuma.core.optics.Format

import java.math.RoundingMode.HALF_UP

object wavelength {

  trait DecoderWavelength {

    given Decoder[Wavelength] =
      Decoder.instance { c =>
        def fromDecimal(field: String, fmt: Format[BigDecimal, Wavelength]): Decoder.Result[Wavelength] =
          c.downField(field).as[BigDecimal].flatMap { bd =>
            fmt.getOption(bd).toRight(DecodingFailure(s"Invalid $field wavelength value: $bd", c.history))
          }

        c.downField("picometers").as[Int].flatMap { pm =>
          Wavelength
            .intPicometers
            .getOption(pm)
            .toRight(DecodingFailure(s"Invalid wavelength picometers value: $pm", c.history))
        } orElse
        fromDecimal("angstroms", Wavelength.decimalAngstroms) orElse
        fromDecimal("nanometers", Wavelength.decimalNanometers) orElse
        fromDecimal("micrometers", Wavelength.decimalMicrometers)
      }

  }

  object decoder extends DecoderWavelength

  trait QueryCodec extends DecoderWavelength {
    given Encoder_Wavelength: Encoder[Wavelength] =
      Encoder.instance { (w: Wavelength) =>
        Json.obj(
          "picometers"  -> w.toPicometers.value.value.asJson,
          "angstroms"   -> Wavelength.decimalAngstroms.reverseGet(w).asJson,
          "nanometers"  -> Wavelength.decimalNanometers.reverseGet(w).asJson,
          "micrometers" -> Wavelength.decimalMicrometers.reverseGet(w).asJson,
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderWavelength {
    given Encoder_Wavelength: Encoder[Wavelength] =
      Encoder.instance { (w: Wavelength) =>
        Json.obj(
          "picometers"  -> w.toPicometers.value.value.asJson
        )
      }
  }

  object transport extends TransportCodec

}

