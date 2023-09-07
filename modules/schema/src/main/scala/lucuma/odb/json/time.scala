// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import lucuma.core.optics.Format
import lucuma.core.util.TimeSpan

object time {

  trait DecoderTimeSpan {

    given Decoder[TimeSpan] =
      Decoder.instance { c =>
        def from[T: Decoder](field: String, format: Format[T, TimeSpan]): Decoder.Result[TimeSpan] =
          c.downField(field).as[T].flatMap { t =>
            format
              .getOption(t)
              .toRight(DecodingFailure(s"Invalid TimeSpan $field: $t", c.history))
          }

        c.downField("microseconds").as[Long].flatMap { µs =>
          TimeSpan
            .FromMicroseconds
            .getOption(µs)
            .toRight(DecodingFailure(s"Invalid TimeSpan microseconds: $µs", c.history))
        } orElse
          from("milliseconds", TimeSpan.FromMilliseconds) orElse
          from("seconds",      TimeSpan.FromSeconds     ) orElse
          from("minutes",      TimeSpan.FromMinutes     ) orElse
          from("hours",        TimeSpan.FromHours       ) orElse
          from("iso",          TimeSpan.FromString      ) orElse
          DecodingFailure(s"Could not parse duration value ${c.value.spaces2}", c.history).asLeft
      }

  }

  object decoder extends DecoderTimeSpan

  trait QueryCodec extends DecoderTimeSpan {
    given Encoder_TimeSpan: Encoder[TimeSpan] =
      Encoder { (ts: TimeSpan) =>
        Json.obj(
          "microseconds" -> TimeSpan.FromMicroseconds.reverseGet(ts).asJson,
          "milliseconds" -> TimeSpan.FromMilliseconds.reverseGet(ts).asJson,
          "seconds"      -> TimeSpan.FromSeconds.reverseGet(ts).asJson,
          "minutes"      -> TimeSpan.FromMinutes.reverseGet(ts).asJson,
          "hours"        -> TimeSpan.FromHours.reverseGet(ts).asJson,
          "iso"          -> TimeSpan.FromString.reverseGet(ts).asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends DecoderTimeSpan {
    given Encoder_TimeSpan: Encoder[TimeSpan] =
      Encoder.instance { (ts: TimeSpan) =>
        Json.obj(
          "microseconds" -> TimeSpan.FromMicroseconds.reverseGet(ts).asJson
        )
      }
  }

  object transport extends TransportCodec

}
