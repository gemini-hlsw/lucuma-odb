// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import cats.data.NonEmptyList
import io.circe.Decoder
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.TelluricType

trait TelluricTypeCodecs:

  // Explicit Circe encoder/decoder matching the GraphQL schema format
  // tag values are uppercase to match the GraphQL TelluricTag enum
  given Encoder[TelluricType] = Encoder.instance:
    case TelluricType.Hot               => 
      Json.obj("tag" -> Json.fromString("HOT"), "starTypes" -> Json.Null)
    case TelluricType.A0V               => 
      Json.obj("tag" -> Json.fromString("A0V"), "starTypes" -> Json.Null)
    case TelluricType.Solar             => 
      Json.obj("tag" -> Json.fromString("SOLAR"), "starTypes" -> Json.Null)
    case TelluricType.Manual(starTypes) => 
      Json.obj("tag" -> Json.fromString("MANUAL"), "starTypes" -> starTypes.asJson)

  given Decoder[TelluricType] = Decoder.instance { cursor =>
    cursor.downField("tag").as[String].flatMap { tag =>
      tag.toUpperCase match {
        case "HOT"   => Right(TelluricType.Hot)
        case "A0V"   => Right(TelluricType.A0V)
        case "SOLAR" => Right(TelluricType.Solar)
        case "MANUAL" =>
          cursor.downField("starTypes").as[Option[List[String]]].flatMap {
            case Some(types) =>
              NonEmptyList.fromList(types) match {
                case Some(nel) => Right(TelluricType.Manual(nel))
                case None => Left(io.circe.DecodingFailure("starTypes must be non-empty for Manual", cursor.history))
              }
            case None => Left(io.circe.DecodingFailure("starTypes is required for Manual", cursor.history))
          }
        case _ => Left(io.circe.DecodingFailure(s"Unknown TelluricType tag: $tag", cursor.history))
      }
    }
  }

object TelluricTypeCodecs extends TelluricTypeCodecs
