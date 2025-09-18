// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.optics.Format
import monocle.Prism

object wavelength {

  trait DecoderWavelength {

    private def wavelengthDecoder[A](
      name:     String,
      pmPrism:  Prism[Int, A],
      aFormat:  Format[BigDecimal, A],
      nmFormat: Format[BigDecimal, A],
      µmFormat: Format[BigDecimal, A]
    ): Decoder[A] =
      Decoder.instance { c =>
        def fromDecimal(field: String, fmt: Format[BigDecimal, A]): Decoder.Result[A] =
          c.downField(field).as[BigDecimal].flatMap { bd =>
            fmt
              .getOption(bd)
              .toRight(DecodingFailure(s"Invalid $field $name value: $bd", c.history))
          }

        c.downField("picometers")
          .as[Int]
          .flatMap { pm =>
            pmPrism
              .getOption(pm)
              .toRight(DecodingFailure(s"Invalid $name picometers value: $pm", c.history))
          }
          .orElse(fromDecimal("angstroms", aFormat))
          .orElse(fromDecimal("nanometers", nmFormat))
          .orElse(fromDecimal("micrometers", µmFormat))
      }

    given Decoder[Wavelength] =
      wavelengthDecoder("wavelength",
                        Wavelength.intPicometers,
                        Wavelength.decimalAngstroms,
                        Wavelength.decimalNanometers,
                        Wavelength.decimalMicrometers
      )

    given Decoder[WavelengthDither] =
      wavelengthDecoder(
        "wavelength dither",
        WavelengthDither.intPicometers,
        WavelengthDither.decimalAngstroms,
        WavelengthDither.decimalNanometers,
        WavelengthDither.decimalMicrometers
      )

  }

  object decoder extends DecoderWavelength

  trait QueryCodec extends DecoderWavelength {
    private def wavelengthEncoder[A](
      pmPrism:  Prism[Int, A],
      aFormat:  Format[BigDecimal, A],
      nmFormat: Format[BigDecimal, A],
      µmFormat: Format[BigDecimal, A]
    ): Encoder[A] =
      Encoder.instance { (a: A) =>
        Json.obj(
          "picometers"  -> pmPrism.reverseGet(a).asJson,
          "angstroms"   -> aFormat.reverseGet(a).asJson,
          "nanometers"  -> nmFormat.reverseGet(a).asJson,
          "micrometers" -> µmFormat.reverseGet(a).asJson
        )
      }

    given Encoder_Wavelength: Encoder[Wavelength] =
      wavelengthEncoder(Wavelength.intPicometers,
                        Wavelength.decimalAngstroms,
                        Wavelength.decimalNanometers,
                        Wavelength.decimalMicrometers
      )

    given Encoder_WavelengthDither: Encoder[WavelengthDither] =
      wavelengthEncoder(WavelengthDither.intPicometers,
                        WavelengthDither.decimalAngstroms,
                        WavelengthDither.decimalNanometers,
                        WavelengthDither.decimalMicrometers
      )
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderWavelength {
    given Encoder_Wavelength: Encoder[Wavelength] =
      Encoder.instance { (w: Wavelength) =>
        Json.obj(
          "picometers" -> w.toPicometers.value.value.asJson
        )
      }

    given Encoder_WavelengthDither: Encoder[WavelengthDither] =
      Encoder.instance { (w: WavelengthDither) =>
        Json.obj(
          "picometers" -> w.toPicometers.value.asJson
        )
      }
  }

  object transport extends TransportCodec

}
