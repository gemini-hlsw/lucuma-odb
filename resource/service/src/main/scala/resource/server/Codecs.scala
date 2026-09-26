// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package resource.server

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Partner
import lucuma.core.model.ProgramReference
import lucuma.core.util.Timestamp
import lucuma.odb.util.CoreCodecs
import resource.model.ComponentLocation
import resource.model.InstrumentComponentType
import resource.model.InstrumentPlace
import resource.model.MoonPhase
import resource.model.PowerSource
import resource.model.ResourceInstrument
import resource.model.ResourceUsage
import resource.model.TelescopeAvailability
import resource.model.TelescopeModeType
import resource.model.TelescopeSubsystem
import resource.model.TooSupport
import skunk.Codec
import skunk.codec.temporal.timestamp
import skunk.codec.text._text
import skunk.data.Arr
import skunk.data.Type

import java.time.LocalDate

/** Codecs for the Resource tables. The ones the ODB shares live in `CoreCodecs`. */
object Codecs extends CoreCodecs:

  val core_timestamp: Codec[Timestamp] =
    timestamp.imap(Timestamp.fromLocalDateTimeTruncatedAndBounded)(_.toLocalDateTime)

  val telescope_availability: Codec[TelescopeAvailability] =
    enumerated(Type("e_telescope_availability"))

  val too_support: Codec[TooSupport] =
    enumerated(Type("e_too_support"))

  val telescope_mode_type: Codec[TelescopeModeType] =
    enumerated(Type("e_telescope_mode_type"))

  /** Resource stores a partner as an `e_partner` enum; the ODB stores it as a `d_tag`. */
  val partner: Codec[Partner] =
    enumerated(Type("e_partner"))

  val program_reference_array: Codec[List[ProgramReference]] =
    _text.eimap(
      _.toList.traverse(s =>
        ProgramReference.fromString.getOption(s).toRight(s"Invalid program reference: '$s'")
      )
    )(l => Arr(l.map(ProgramReference.fromString.reverseGet)*))

  val moon_phase: Codec[MoonPhase] =
    enumerated(Type("e_moon_phase"))

  val resource_instrument: Codec[ResourceInstrument] =
    enumerated(Type("e_resource_instrument"))

  val instrument_place: Codec[InstrumentPlace] =
    enumerated(Type("e_instrument_place"))

  val resource_usage: Codec[ResourceUsage] =
    enumerated(Type("e_resource_usage"))

  val telescope_subsystem: Codec[TelescopeSubsystem] =
    enumerated(Type("e_telescope_subsystem"))

  val power_source: Codec[PowerSource] =
    enumerated(Type("e_power_source"))

  val instrument_component_type: Codec[InstrumentComponentType] =
    enumerated(Type("e_instrument_component_type"))

  val component_location: Codec[ComponentLocation] =
    enumerated(Type("e_component_location"))

  val nonempty_text_array: Codec[List[NonEmptyString]] =
    _text.eimap(
      _.toList.traverse(s => NonEmptyString.from(s))
    )(l => Arr(l.map(_.value)*))

  val date_array: Codec[List[LocalDate]] =
    Codec
      .array[LocalDate](
        _.toString,
        s => Either.catchNonFatal(LocalDate.parse(s)).leftMap(_ => s"Invalid date: '$s'"),
        Type("_date", List(Type("date")))
      )
      .imap(_.toList)(l => Arr(l*))
