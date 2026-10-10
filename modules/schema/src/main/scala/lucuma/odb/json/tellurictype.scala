// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyList
import cats.syntax.either.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.TelluricCount
import lucuma.core.model.TelluricType

object tellurictype:

  trait DecoderTelluricType:
    given Decoder[TelluricType] = Decoder.instance: cursor =>
      cursor.downField("tag").as[String].flatMap: tag =>
        tag.toUpperCase match
          case "HOT"                     => TelluricType.Hot.asRight
          case "A0V"                     => TelluricType.A0V.asRight
          case "SOLAR"                   => TelluricType.Solar.asRight
          case "NO_TELLURIC"             => TelluricType.NoTelluric.asRight
          case "EXPLICIT_SPECTRAL_TYPES" =>
            cursor.downField("starTypes").as[Option[List[String]]].flatMap:
              case Some(types) =>
                NonEmptyList.fromList(types) match {
                  case Some(nel) => TelluricType.ExplicitSpectralTypes(nel).asRight
                  case None      => DecodingFailure("starTypes must be non-empty for ExplicitSpectralTypes", cursor.history).asLeft
                }
              case None        =>
                DecodingFailure("starTypes is required for ExplicitSpectralTypes", cursor.history).asLeft
          case "USER_DEFINED"            =>
            cursor.downField("count").as[Option[Int]].flatMap:
              case Some(count) =>
                TelluricCount.from(count)
                  .leftMap(_ => DecodingFailure("count must be 1 or 2 for UserDefined", cursor.history))
                  .map(TelluricType.UserDefined(_))
              case None        =>
                DecodingFailure("count is required for UserDefined", cursor.history).asLeft
          case _                         => DecodingFailure(s"Unknown TelluricType tag: $tag", cursor.history).asLeft

  object decoder extends DecoderTelluricType

  trait QueryCodec extends DecoderTelluricType:
    given Encoder_TelluricType: Encoder[TelluricType] =
      Encoder.instance:
        case TelluricType.Hot                              =>
          Json.obj("tag" -> Json.fromString("HOT"), "starTypes" -> Json.Null, "count" -> Json.Null)
        case TelluricType.A0V                              =>
          Json.obj("tag" -> Json.fromString("A0V"), "starTypes" -> Json.Null, "count" -> Json.Null)
        case TelluricType.Solar                            =>
          Json.obj("tag" -> Json.fromString("SOLAR"), "starTypes" -> Json.Null, "count" -> Json.Null)
        case TelluricType.NoTelluric                       =>
          Json.obj("tag" -> Json.fromString("NO_TELLURIC"), "starTypes" -> Json.Null, "count" -> Json.Null)
        case TelluricType.ExplicitSpectralTypes(starTypes) =>
          Json.obj("tag" -> Json.fromString("EXPLICIT_SPECTRAL_TYPES"), "starTypes" -> starTypes.asJson, "count" -> Json.Null)
        case TelluricType.UserDefined(count)               =>
          Json.obj("tag" -> Json.fromString("USER_DEFINED"), "starTypes" -> Json.Null, "count" -> Json.fromInt(count.value.value))

  object query extends QueryCodec

  trait TransportCodec extends DecoderTelluricType:
    given Encoder_TelluricType: Encoder[TelluricType] =
      Encoder.instance:
        case TelluricType.Hot                              =>
          Json.obj("tag" -> Json.fromString("HOT"))
        case TelluricType.A0V                              =>
          Json.obj("tag" -> Json.fromString("A0V"))
        case TelluricType.Solar                            =>
          Json.obj("tag" -> Json.fromString("SOLAR"))
        case TelluricType.NoTelluric                       =>
          Json.obj("tag" -> Json.fromString("NO_TELLURIC"))
        case TelluricType.ExplicitSpectralTypes(starTypes) =>
          Json.obj("tag" -> Json.fromString("EXPLICIT_SPECTRAL_TYPES"), "starTypes" -> starTypes.asJson)
        case TelluricType.UserDefined(count)               =>
          Json.obj("tag" -> Json.fromString("USER_DEFINED"), "count" -> Json.fromInt(count.value.value))

  object transport extends TransportCodec
