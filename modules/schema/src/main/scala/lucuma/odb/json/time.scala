// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.syntax.either.*
import cats.syntax.order.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.optics.Format
import lucuma.core.util.DateInterval
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import org.typelevel.cats.time.given

import java.time.LocalDate

object time {

  trait TimeDecoders {

    given Decoder[DateInterval] = c =>
      for {
        s <- c.downField("start").as[LocalDate]
        e <- c.downField("end").as[LocalDate]
        _ <- Either.raiseWhen(s > e)(DecodingFailure(s"'start' ($s) after 'end' ($e)", c.history))
      } yield DateInterval.between(s, e)

    given Decoder[TimeSpan] = c =>
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

    given Decoder[TimestampInterval] = c =>
      for {
        s <- c.downField("start").as[Timestamp]
        e <- c.downField("end").as[Timestamp]
        _ <- Either.raiseWhen(s > e)(DecodingFailure(s"'start' ($s) after 'end' ($e)", c.history))
      } yield TimestampInterval.between(s, e)

  }

  object decoder extends TimeDecoders

  trait CommonEncoders {
    given Encoder[DateInterval] =
      Encoder.instance { (a: DateInterval) =>
        Json.obj(
          "start" -> a.start.asJson,
          "end"   -> a.end.asJson
        )
      }
  }

  trait QueryCodec extends TimeDecoders with CommonEncoders {
    given Encoder_TimeSpan: Encoder[TimeSpan] =
      Encoder.instance { (ts: TimeSpan) =>
        Json.obj(
          "microseconds" -> TimeSpan.FromMicroseconds.reverseGet(ts).asJson,
          "milliseconds" -> TimeSpan.FromMilliseconds.reverseGet(ts).asJson,
          "seconds"      -> TimeSpan.FromSeconds.reverseGet(ts).asJson,
          "minutes"      -> TimeSpan.FromMinutes.reverseGet(ts).asJson,
          "hours"        -> TimeSpan.FromHours.reverseGet(ts).asJson,
          "iso"          -> TimeSpan.FromString.reverseGet(ts).asJson
        )
      }

    given Encoder_TimestampInterval: Encoder[TimestampInterval] =
      Encoder.instance { (a: TimestampInterval) =>
        Json.obj(
          "start"    -> a.start.asJson,
          "end"      -> a.end.asJson,
          "duration" -> TimeSpan.between(a.start, a.end).asJson
        )
      }
  }

  object query extends QueryCodec

  trait TransportCodec extends TimeDecoders with CommonEncoders {
    given Encoder_TimeSpan: Encoder[TimeSpan] =
      Encoder.instance { (ts: TimeSpan) =>
        Json.obj(
          "microseconds" -> TimeSpan.FromMicroseconds.reverseGet(ts).asJson
        )
      }

    given Encoder_TimestampInterval: Encoder[TimestampInterval] =
      Encoder.instance { (a: TimestampInterval) =>
        Json.obj(
          "start"    -> a.start.asJson,
          "end"      -> a.end.asJson
        )
      }

  }

  object transport extends TransportCodec

}
