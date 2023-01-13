// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json


import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._
import lucuma.core.math.Wavelength

trait WavelengthCodec {
  given Codec[Wavelength] with {
    def apply(w: Wavelength): Json =
      Json.obj(
        "picometers"  -> w.toPicometers.value.value.asJson,
        "angstroms"   -> Wavelength.decimalAngstroms.reverseGet(w).asJson,
        "nanometers"  -> Wavelength.decimalNanometers.reverseGet(w).asJson,
        "micrometers" -> Wavelength.decimalMicrometers.reverseGet(w).asJson,
      )

    def apply(c: HCursor): Decoder.Result[Wavelength] =
      c.downField("picometers").as[Int].flatMap { pm =>
        Wavelength
          .intPicometers
          .getOption(pm)
          .toRight(DecodingFailure(s"Invalid wavelength picometers value: $pm", c.history))
      }
  }

}

object wavelength extends WavelengthCodec

