// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service.database

import lucuma.core.enums.Partner
import lucuma.core.model.*
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.sso.client.ApiKey
import lucuma.sso.service.SessionToken
import skunk.*
import skunk.codec.all.*
import skunk.data.Type

// Codecs for some atomic types.
trait Codecs {

  val orcid_id: Codec[OrcidId] =
    Codec.simple[OrcidId](
      _.value.toString(),
      OrcidId.fromValue(_),
      Type.varchar
    )

  val user_id: Codec[User.Id] = {
    val prism = Gid[User.Id].fromString
    Codec.simple(
      prism.reverseGet,
      s => prism.getOption(s).toRight(s"Invalid user id: $s"),
      Type.varchar
    )
  }

  val role_id: Codec[StandardRole.Id] = {
    val prism = Gid[StandardRole.Id].fromString
    Codec.simple(
      prism.reverseGet,
      s => prism.getOption(s).toRight(s"Invalid role id: $s"),
      Type.varchar
    )
  }

  private def toPrefix(x: String): String = (x.toList match {
    case x :: tail => x.toUpper :: tail
    case x => x
  }).mkString("")

  def enums[A](encode: A => String, decode: String => Option[A], tpe: Type): Codec[A] =
    Codec.simple[A](encode, s => decode(s).toRight(s"${tpe.name}: no such element '$s'"), tpe)

  val role_type: Codec[RoleType] =
    enums[RoleType](Enumerated[RoleType].tag(_).toLowerCase, x => Enumerated[RoleType].fromTag(toPrefix(x)), Type("lucuma_role_type"))

  val partner: Codec[Partner] =
    enums[Partner](Enumerated[Partner].tag(_).toLowerCase, x => Enumerated[Partner].all.find(_.tag.equalsIgnoreCase(x)), Type("lucuma_ngo"))

  val session_token: Codec[SessionToken] =
    uuid.to

  val api_key: Decoder[ApiKey] =
    text.map(ApiKey.fromString.getOption).emap(_.toRight("Invalid API Key"))

}

object Codecs extends Codecs
