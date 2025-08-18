// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.client.codec

import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.*

trait UserCodec {
  import gid._
  import role._
  import orcidProfile.given

  implicit val EncoderUser: Encoder[User] =
    Encoder.instance {

      case GuestUser(id) =>
        Json.obj(
          "type" -> "guest".asJson,
          "id"   -> id.asJson
        )
      case ServiceUser(id, name) =>
        Json.obj(
          "type" -> "service".asJson,
          "id"   -> id.asJson,
          "name" -> name.asJson,
        )
      case StandardUser(id, role, otherRoles, profile) =>
        Json.obj(
          "type"       -> "standard".asJson,
          "id"         -> id.asJson,
          "role"       -> role.asJson,
          "otherRoles" -> otherRoles.asJson,
          "profile"    -> profile.asJson,
        )

    }

  implicit val DecodeUser: Decoder[User] = hc =>
    hc.downField("type").as[String].flatMap {

      case "guest" =>
        for {
          id <- hc.downField("id").as[User.Id]
        } yield GuestUser(id)

      case "service" =>
        for {
          id <- hc.downField("id").as[User.Id]
          n  <- hc.downField("name").as[String]
        } yield ServiceUser(id, n)

      case "standard" =>
        for {
          id <- hc.downField("id").as[User.Id]
          role <- hc.downField("role").as[StandardRole]
          otherRoles <- hc.downField("otherRoles").as[List[StandardRole]]
          profile <- hc.downField("profile").as[OrcidProfile]
        } yield StandardUser(id, role, otherRoles, profile)

      case tag  =>
        Left(DecodingFailure(s"Unknown user type: $tag", Nil))

    }

}

object user extends UserCodec
