// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.codec

import cats.syntax.all.*
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardRole.*

trait RoleCodec {
  import gid._

  implicit val EncoderStandardRole: Encoder[StandardRole] = {
    case Pi(id)     => Json.obj("type" -> "pi".asJson,    "id" -> id.asJson)
    case Ngo(id, p) => Json.obj("type" -> "ngo".asJson,   "id" -> id.asJson, "partner" -> p.asJson)
    case Staff(id)  => Json.obj("type" -> "staff".asJson, "id" -> id.asJson)
    case Admin(id)  => Json.obj("type" -> "admin".asJson, "id" -> id.asJson)
  }

  implicit val DecoderStandardRole: Decoder[StandardRole] = hc =>
    (hc.downField("type").as[String], hc.downField("id").as[Id], hc.downField("partner").as[Option[Partner]])
      .tupled.flatMap {
        case ("pi",    id, None)    => Pi(id).asRight
        case ("ngo",   id, Some(p)) => Ngo(id, p).asRight
        case ("staff", id, None)    => Staff(id).asRight
        case ("admin", id, None)    => Admin(id).asRight
        case _ => DecodingFailure(s"Invalid StandardRole: ${hc.as[Json]}", hc.history).asLeft
      }

}

object role extends RoleCodec
