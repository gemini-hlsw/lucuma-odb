package lucuma.odb.util

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import lucuma.core.model._
import lucuma.core.util.Gid
import skunk._
import skunk.data.Type
import lucuma.core.enum.Site
import skunk.codec.all._
import skunk.data.Arr
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.util.Enumerated
import lucuma.odb.data.Existence
import lucuma.odb.data.UserType
import lucuma.odb.data.ProgramUserRole

// Codecs for some atomic types.
trait Codecs {

  val text_nonempty: Codec[NonEmptyString] =
    text.eimap(NonEmptyString.from)(_.value)

  val orcid_id: Codec[OrcidId] =
    Codec.simple[OrcidId](
      _.value.toString(),
      OrcidId.fromValue(_),
      Type.varchar
    )

  def gid[A](implicit ev: Gid[A]): Codec[A] = {
    val prism = ev.fromString
    Codec.simple(
      prism.reverseGet,
      s => prism.getOption(s).toRight(s"Invalid: $s"),
      Type.varchar
    )
  }

  def enumerated[A](tpe: Type)(implicit ev: Enumerated[A]): Codec[A] =
    `enum`(ev.tag, ev.fromTag, tpe)

  val user_id: Codec[User.Id] = gid[User.Id]
  val program_id: Codec[Program.Id] = gid[Program.Id]

  val site: Codec[Site] =
    `enum`(_.tag.toLowerCase, s => Site.fromTag(s.toUpperCase), Type("e_site"))

  val user_type: Codec[UserType] =
    enumerated(Type("e_user_type"))

  val program_user_role: Codec[ProgramUserRole] =
    enumerated(Type("e_program_user_role"))

  val _site: Codec[Arr[Site]] =
    Codec.array(_.tag.toLowerCase, s => Site.fromTag(s.toUpperCase).toRight(s"Invalid tag: $s"), Type("_e_site"))

  val existence: Codec[Existence] =
    enumerated(Type("e_existence"))

}

object Codecs extends Codecs