// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.bifunctor.*
import cats.syntax.either.*
import io.circe.Codec
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.refined._
import io.circe.syntax._

import java.math.RoundingMode.DOWN
import java.time.Duration
import java.time.temporal.ChronoUnit.MICROS
import scala.util.Try

object time {

  // N.B. We cannot represent any random `Duration` with the existing GraphQL
  // schema.  Only a Duration expressed as a `Long` in milliseconds can be
  // represented.  We need to either change the schema or make a specialized
  // duration type.

  trait DecoderTime {

    given Decoder[Duration] =
      Decoder.instance { c =>
        def fromDecimal(field: String, µs: Long): Decoder.Result[Duration] =
          c.downField(field).as[BigDecimal].map { bd =>
            val decMicro = (bd * µs).bigDecimal
            val decSecs  = decMicro.movePointLeft(6)
            val longSecs = decSecs.setScale(0, DOWN)
            val longNano = (decSecs.subtract(longSecs)).movePointRight(6).setScale(0, DOWN).movePointRight(3)
            Duration.ofSeconds(longSecs.longValueExact, longNano.longValueExact)
          }

        c.downField("microseconds").as[Long].map { µs =>
          val secs = µs / 1_000_000L
          val nano = (µs % 1_000_000L) * 1_000L
          Duration.ofSeconds(secs, nano)
        } orElse
          fromDecimal("milliseconds",  1_000L) orElse
          fromDecimal("seconds",   1_000_000L) orElse
          fromDecimal("minutes",  60_000_000L) orElse
          fromDecimal("hours", 3_600_000_000L) orElse
          c.downField("iso").as[String].flatMap { iso =>
            Try(Duration.parse(iso)).toEither.leftMap(_ => DecodingFailure(s"Could not parse `$iso` as a duration", c.history))
          } orElse
          DecodingFailure(s"Could not parse duration value", c.history).asLeft
      }

  }

  object decoder extends DecoderTime

  private def microseconds(d: Duration): BigDecimal = {
    val d2 = d.truncatedTo(MICROS)

    BigDecimal(
      new java.math.BigDecimal(d2.getSeconds)
        .movePointRight(9)
        .add(new java.math.BigDecimal(d2.getNano))
        .movePointLeft(3)
        .setScale(0, DOWN)
    )
  }


  trait QueryCodec extends DecoderTime {
    given Encoder_Duration: Encoder[Duration] =
      Encoder { (d: Duration) =>
        val µs = microseconds(d)

        Json.obj(
          "microseconds" -> µs.longValue.asJson,
          "milliseconds" -> (µs /         1_000L).asJson,
          "seconds"      -> (µs /     1_000_000L).asJson,
          "minutes"      -> (µs /    60_000_000L).asJson,
          "hours"        -> (µs / 3_600_000_000L).asJson,
          "iso"          -> µs.toString.asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderTime {
    given Encoder_Duration: Encoder[Duration] =
      Encoder.instance { (d: Duration) =>
        Json.obj(
          "microseconds" -> microseconds(d).longValue.asJson
        )
      }
  }

  object transport extends TransportCodec

}


