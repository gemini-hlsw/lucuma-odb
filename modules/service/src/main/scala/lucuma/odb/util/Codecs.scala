// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
import lucuma.odb.data.Tag
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.ObsActiveStatus
import lucuma.odb.data.ObsStatus
import lucuma.core.math.Angle
import lucuma.core.math.RightAscension
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.RadialVelocity
import lucuma.core.enum.CatalogName
import lucuma.core.enum.EphemerisKeyType
import lucuma.core.math.Parallax

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
  val target_id: Codec[Target.Id] = gid[Target.Id]
  val program_id: Codec[Program.Id] = gid[Program.Id]
  val observation_id: Codec[Observation.Id] = gid[Observation.Id]

  val site: Codec[Site] =
    `enum`(_.tag.toLowerCase, s => Site.fromTag(s.toUpperCase), Type("e_site"))

  val user_type: Codec[UserType] =
    enumerated(Type("e_user_type"))

  val program_user_role: Codec[ProgramUserRole] =
    enumerated(Type("e_program_user_role"))

  val program_user_support_type: Codec[ProgramUserSupportType] =
    enumerated(Type("e_program_user_support_type"))

  val _site: Codec[Arr[Site]] =
    Codec.array(_.tag.toLowerCase, s => Site.fromTag(s.toUpperCase).toRight(s"Invalid tag: $s"), Type("_e_site"))

  val existence: Codec[Existence] =
    enumerated(Type("e_existence"))

  val tag: Codec[Tag] =
    varchar.gimap

  val obs_status: Codec[ObsStatus] =
    enumerated(Type("e_obs_status"))

  val obs_active_status: Codec[ObsActiveStatus] =
    enumerated(Type("e_obs_active_status"))

  val angle_µas: Codec[Angle] =
    int8.imap(Angle.microarcseconds.reverseGet)(Angle.microarcseconds.get)

  val right_ascension: Codec[RightAscension] =
    angle_µas.eimap(
      a => RightAscension.fromAngleExact.getOption(a).toRight(s"Invalid right ascension: $a"))(
      RightAscension.fromAngleExact.reverseGet
    )

  val declination: Codec[Declination] =
    angle_µas.eimap(
      a => Declination.fromAngle.getOption(a).toRight(s"Invalied declination: $a"))(
      Declination.fromAngle.reverseGet
    )

  val epoch: Codec[Epoch] =
    varchar.eimap(
      s => Epoch.fromString.getOption(s).toRight(s"Invalid epoch: $s"))(
      Epoch.fromString.reverseGet
    )

  val radial_velocity: Codec[RadialVelocity] =
    numeric.eimap(
      bd => RadialVelocity.kilometerspersecond.getOption(bd).toRight(s"Invalid radial velocity: $bd"))(
      RadialVelocity.kilometerspersecond.reverseGet
    )

  val catalog_name: Codec[CatalogName] =
    enumerated(Type("e_catalog_name"))

  val ephemeris_key_type: Codec[EphemerisKeyType] =
    enumerated(Type("e_ephemeris_key_type"))

  val parallax: Codec[Parallax] =
    angle_µas.imap(
      a => Parallax.fromMicroarcseconds(a.toMicroarcseconds))(
      p => Angle.fromMicroarcseconds(p.μas.value.value)
    )

}

object Codecs extends Codecs