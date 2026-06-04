// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import lucuma.core.enums.Site
import lucuma.core.model.ProgramReference
import lucuma.core.util.Enumerated
import lucuma.core.util.Timestamp
import resource.model.TelescopeAvailability
import resource.model.TelescopeModeType
import resource.model.TooSupport
import skunk.Codec
import skunk.codec.temporal.timestamp
import skunk.codec.text.text
import skunk.data.Type

object Codecs:

  def enumerated[A](tpe: Type)(implicit ev: Enumerated[A]): Codec[A] =
    `enum`(ev.tag, ev.fromTag, tpe)

  private def `enum`[A](encode: A => String, decode: String => Option[A], tpe: Type): Codec[A] =
    Codec.simple(encode, s => decode(s).toRight(s"${tpe.name}: no such element '$s'"), tpe)

  val core_timestamp: Codec[Timestamp] =
    timestamp.imap(Timestamp.fromLocalDateTimeTruncatedAndBounded)(_.toLocalDateTime)

  val site: Codec[Site] =
    `enum`(_.tag.toLowerCase, s => Enumerated[Site].fromTag(s.toUpperCase), Type("e_site"))

  val telescope_availability: Codec[TelescopeAvailability] =
    enumerated(Type("e_telescope_availability"))

  val too_support: Codec[TooSupport] =
    enumerated(Type("e_too_support"))

  val telescope_mode_type: Codec[TelescopeModeType] =
    enumerated(Type("e_telescope_mode_type"))

  val program_reference: Codec[ProgramReference] =
    text.eimap(s =>
      ProgramReference.fromString.getOption(s).toRight(s"Invalid program reference: $s")
    )(ProgramReference.fromString.reverseGet)
