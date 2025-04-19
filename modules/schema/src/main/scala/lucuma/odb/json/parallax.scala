// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Parallax

import java.math.MathContext

object parallax {

  trait DecoderParallax {
    given Decoder[Parallax] =
      Decoder.instance { c =>
        c.downField("microarcseconds")
          .as[Long]
          .map(Parallax.microarcseconds.reverseGet)
          .orElse(c.downField("milliarcseconds").as[BigDecimal].map(Parallax.milliarcseconds.reverseGet))
          .orElse(DecodingFailure("Could not parse parallax", c.history).asLeft)
      }
  }

  object decoder extends DecoderParallax

  trait QueryCodec extends DecoderParallax {

    given Encoder_Parallax: Encoder[Parallax] =
      Encoder.instance { p =>
        Json.obj(
          "microarcseconds" -> p.μas.value.value.asJson,
          "milliarcseconds" -> p.mas.value.toBigDecimal(MathContext.DECIMAL128).asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderParallax {

    given Encoder_Parallax: Encoder[Parallax] =
      Encoder.instance { p =>
        Json.obj(
          "microarcseconds" -> p.μas.value.value.asJson
        )
      }
  }

  object transport extends TransportCodec
}
