// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.ProperMotion

object propermotion {

  trait DecoderProperMotion {
    given Decoder[ProperMotion.RA] =
      Decoder.instance { c =>
        c.downField("microarcsecondsPerYear")
          .as[Long]
          .map(ProperMotion.RA.microarcsecondsPerYear.get)
          .orElse(
            c.downField("milliarcsecondsPerYear").as[BigDecimal].map(ProperMotion.RA.milliarcsecondsPerYear.reverseGet)
          )
          .orElse(DecodingFailure("Could not parse propermotion.ra", c.history).asLeft)
      }

    given Decoder[ProperMotion.Dec] =
      Decoder.instance { c =>
        c.downField("microarcsecondsPerYear")
          .as[Long]
          .map(ProperMotion.Dec.microarcsecondsPerYear.get)
          .orElse(
            c.downField("milliarcsecondsPerYear").as[BigDecimal].map(ProperMotion.Dec.milliarcsecondsPerYear.reverseGet)
          )
          .orElse(DecodingFailure("Could not parse propermotion.dec", c.history).asLeft)
      }

    given Decoder[ProperMotion] =
      Decoder.instance { c =>
        for {
          ra  <- c.downField("ra").as[ProperMotion.RA]
          dec <- c.downField("dec").as[ProperMotion.Dec]
        } yield ProperMotion(ra, dec)
      }
  }

  object decoder extends DecoderProperMotion

  trait QueryCodec extends DecoderProperMotion {

    given Encoder_ProperMotion_RA: Encoder[ProperMotion.RA] =
      Encoder.instance { ra =>
        Json.obj(
          "microarcsecondsPerYear" -> ProperMotion.RA.microarcsecondsPerYear.reverseGet(ra).asJson,
          "milliarcsecondsPerYear" -> ProperMotion.RA.milliarcsecondsPerYear.get(ra).asJson
        )
      }

    given Encoder_ProperMotion_Dec: Encoder[ProperMotion.Dec] =
      Encoder.instance { dec =>
        Json.obj(
          "microarcsecondsPerYear" -> ProperMotion.Dec.microarcsecondsPerYear.reverseGet(dec).asJson,
          "milliarcsecondsPerYear" -> ProperMotion.Dec.milliarcsecondsPerYear.get(dec).asJson
        )
      }

    given Encoder_ProperMotion: Encoder[ProperMotion] =
      Encoder.instance { pm =>
        Json.obj(
          "ra"  -> pm.ra.asJson,
          "dec" -> pm.dec.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderProperMotion {

    given Encoder_ProperMotion_RA: Encoder[ProperMotion.RA] =
      Encoder.instance { ra =>
        Json.obj(
          "microarcsecondsPerYear" -> ProperMotion.RA.microarcsecondsPerYear.reverseGet(ra).asJson,
        )
      }

    given Encoder_ProperMotion_Dec: Encoder[ProperMotion.Dec] =
      Encoder.instance { dec =>
        Json.obj(
          "microarcsecondsPerYear" -> ProperMotion.Dec.microarcsecondsPerYear.reverseGet(dec).asJson,
        )
      }

    given Encoder_ProperMotion: Encoder[ProperMotion] =
      Encoder.instance { pm =>
        Json.obj(
          "ra"  -> pm.ra.asJson,
          "dec" -> pm.dec.asJson
        )
      }
  }

  object transport extends TransportCodec
}
