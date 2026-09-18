// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.util.Enumerated
import lucuma.odb.data.Existence
import skunk.Codec
import skunk.codec.numeric.int4
import skunk.codec.text.text
import skunk.codec.text.varchar
import skunk.data.Type

/**
 * Skunk codecs that both the ODB and the Resource service read and write. Each codec is defined
 * once here, so a fix reaches both services.
 */
trait CoreCodecs {

  /** A Postgres enum whose labels are the Scala `Enumerated` tags. */
  def enumerated[A](tpe: Type)(implicit ev: Enumerated[A]): Codec[A] =
    tagged(ev.tag, ev.fromTag, tpe)

  /** A Postgres enum whose labels need a spelling of their own. */
  protected def tagged[A](encode: A => String, decode: String => Option[A], tpe: Type): Codec[A] =
    Codec.simple(encode, s => decode(s).toRight(s"${tpe.name}: no such element '$s'"), tpe)

  val existence: Codec[Existence] =
    enumerated(Type("e_existence"))

  val int4_pos: Codec[PosInt] =
    int4.eimap(PosInt.from)(_.value)

  val semester: Codec[Semester] =
    varchar.eimap(
      s => Semester.fromString.getOption(s).toRight(s"Invalid semester: $s"))(
      _.format
    )

  /** The `e_site` labels are the lower-case Site tags. */
  val site: Codec[Site] =
    tagged(_.tag.toLowerCase, s => Enumerated[Site].fromTag(s.toUpperCase), Type("e_site"))

  val text_nonempty: Codec[NonEmptyString] =
    text.eimap(NonEmptyString.from)(_.value)

}

object CoreCodecs extends CoreCodecs
