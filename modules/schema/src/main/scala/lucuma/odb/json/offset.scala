// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.optics.SplitMono

import java.math.RoundingMode.HALF_UP

object offset {

  def µas[A]: SplitMono[Offset.Component[A], Long] =
    Angle.signedMicroarcseconds.reverse.andThen(Offset.Component.angle[A].reverse).reverse

  trait DecoderOffset {

    private def µasDecoder[A]: Decoder[Offset.Component[A]] =
      Decoder.instance(_.downField("microarcseconds").as[Long].map(µas.reverseGet))

    private def roundingDecoder[A](field: String, right: Int): Decoder[Offset.Component[A]] =
      Decoder.instance(
        _.downField(field)
         .as[BigDecimal]
         .map { bd =>
           µas.reverseGet(
             bd.bigDecimal.movePointRight(right).setScale(0, HALF_UP).longValueExact
           )
         }
      )

    given [A]: Decoder[Offset.Component[A]] =
      µasDecoder                             or
        roundingDecoder("milliarcseconds",3) or
        roundingDecoder("arcseconds",     6)

    given Decoder[Offset] =
      Decoder.instance { c =>
        for {
          p <- c.downField("p").as[Offset.P]
          q <- c.downField("q").as[Offset.Q]
        } yield Offset(p, q)
      }

  }

  object decoder extends DecoderOffset

  trait QueryCodec extends DecoderOffset {

    given Encoder_Offset_Component[A]: Encoder[Offset.Component[A]] =
      Encoder.instance { a =>
        Json.obj(
          "microarcseconds" -> µas.get(a).asJson,
          "milliarcseconds" -> BigDecimal(µas.get(a), 3).asJson,
          "arcseconds"      -> BigDecimal(µas.get(a), 6).asJson
        )
      }

    given Encoder_Offset: Encoder[Offset] =
      Encoder.instance { a =>
        Json.obj(
          "p" -> a.p.asJson,
          "q" -> a.q.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderOffset {

    given Encoder_Offset_Component[A]: Encoder[Offset.Component[A]] =
      Encoder.instance { a =>
        Json.obj(
          "microarcseconds" -> µas.get(a).asJson
        )
      }

    given Encoder_Offset: Encoder[Offset] =
      Encoder.instance { a =>
        Json.obj(
          "p" -> a.p.asJson,
          "q" -> a.q.asJson
        )
      }

  }

  object transport extends TransportCodec
}
