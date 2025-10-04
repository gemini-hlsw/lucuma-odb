// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyList
import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.TelluricType

object tellurictype:

  trait DecoderTelluricType:
    given Decoder[TelluricType] = Decoder.instance: cursor =>
      cursor.downField("tag").as[String].flatMap: tag =>
        tag.toUpperCase match
          case "HOT"    => TelluricType.Hot.asRight
          case "A0V"    => TelluricType.A0V.asRight
          case "SOLAR"  => TelluricType.Solar.asRight
          case "MANUAL" =>
            cursor.downField("starTypes").as[Option[List[String]]].flatMap:
              case Some(types) =>
                NonEmptyList.fromList(types) match {
                  case Some(nel) => TelluricType.Manual(nel).asRight
                  case None      => DecodingFailure("starTypes must be non-empty for Manual", cursor.history).asLeft
                }
              case None        =>
                DecodingFailure("starTypes is required for Manual", cursor.history).asLeft
          case _        => DecodingFailure(s"Unknown TelluricType tag: $tag", cursor.history).asLeft

  object decoder extends DecoderTelluricType

  trait QueryCodec extends DecoderTelluricType:
    given Encoder_TelluricType: Encoder[TelluricType] =
      Encoder.instance:
        case TelluricType.Hot               =>
          Json.obj("tag" -> Json.fromString("HOT"), "starTypes" -> Json.Null)
        case TelluricType.A0V               =>
          Json.obj("tag" -> Json.fromString("A0V"), "starTypes" -> Json.Null)
        case TelluricType.Solar             =>
          Json.obj("tag" -> Json.fromString("SOLAR"), "starTypes" -> Json.Null)
        case TelluricType.Manual(starTypes) =>
          Json.obj("tag" -> Json.fromString("MANUAL"), "starTypes" -> starTypes.asJson)

  object query extends QueryCodec

  trait TransportCodec extends DecoderTelluricType:
    given Encoder_TelluricType: Encoder[TelluricType] =
      Encoder.instance:
        case TelluricType.Hot   =>
          Json.obj("tag" -> Json.fromString("HOT"))
        case TelluricType.A0V   =>
          Json.obj("tag" -> Json.fromString("A0V"))
        case TelluricType.Solar =>
          Json.obj("tag" -> Json.fromString("SOLAR"))
        case TelluricType.Manual(starTypes) =>
          Json.obj("tag" -> Json.fromString("MANUAL"), "starTypes" -> starTypes.asJson)

  object transport extends TransportCodec
