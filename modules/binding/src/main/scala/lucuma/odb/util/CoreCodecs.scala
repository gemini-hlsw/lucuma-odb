// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Site
import lucuma.core.model.Semester
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.Existence
import skunk.Codec
import skunk.codec.`enum`.`enum`
import skunk.codec.numeric.int4
import skunk.codec.temporal.timestamp
import skunk.codec.text.text
import skunk.codec.text.varchar
import skunk.data.Type

/**
 * Skunk codecs that both the ODB and the Resource use
 */
trait CoreCodecs {

  /** A Postgres enum whose labels are the Scala `Enumerated` tags. */
  def enumerated[A](tpe: Type)(using ev: Enumerated[A]): Codec[A] =
    `enum`(ev.tag, ev.fromTag, tpe)

  val core_timestamp: Codec[Timestamp] =
    timestamp.imap(Timestamp.fromLocalDateTimeTruncatedAndBounded)(_.toLocalDateTime)

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
    `enum`(_.tag.toLowerCase, s => Enumerated[Site].fromTag(s.toUpperCase), Type("e_site"))

  val text_nonempty: Codec[NonEmptyString] =
    text.eimap(NonEmptyString.from)(_.value)

  /** A [start, end) interval stored as two timestamp columns. */
  val timestamp_interval: Codec[TimestampInterval] =
    (core_timestamp *: core_timestamp).imap { case (min, max) =>
      TimestampInterval.between(min, max)
    } { interval => (interval.start, interval.end) }

}

object CoreCodecs extends CoreCodecs
