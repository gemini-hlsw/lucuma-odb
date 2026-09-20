// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.AltairNdFilter
import lucuma.core.enums.FieldLens
import lucuma.core.enums.Instrument
import lucuma.odb.data.AltairConfiguration
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*

// Rules governing where an Altair configuration may be attached and what it may say.
object AltairRules:

  val LgsFieldLensMessage: String =
    "Altair LGS modes always use the field lens; fieldLens must be IN or omitted."

  val LgsNdFilterMessage: String =
    "The Altair ND filter is not commissioned for the LGS modes; ndFilter must be OUT."

  def notAvailableMessage(instrument: Instrument): String =
    s"Altair is not available for ${instrument.longName}."

  def checkInstrument(instrument: Instrument, altairInstruments: Set[Instrument], prefix: String = ""): Result[Unit] =
    if altairInstruments.contains(instrument) then Result.unit
    else OdbError.InvalidArgument(s"$prefix${notAvailableMessage(instrument)}".some).asFailure

  def checkFieldLens(altair: AltairConfiguration, prefix: String = ""): Result[Unit] =
    if altair.mode.usesLaser && altair.explicitFieldLens.contains(FieldLens.Out)
    then OdbError.InvalidArgument(s"$prefix$LgsFieldLensMessage".some).asFailure
    else Result.unit

  def checkNdFilter(altair: AltairConfiguration, prefix: String = ""): Result[Unit] =
    if altair.mode.usesLaser && altair.ndFilter === AltairNdFilter.In
    then OdbError.InvalidArgument(s"$prefix$LgsNdFilterMessage".some).asFailure
    else Result.unit

  /** The input checks that need nothing but the Altair configuration itself. */
  def checkConfiguration(altair: AltairConfiguration, prefix: String = ""): Result[Unit] =
    (checkFieldLens(altair, prefix), checkNdFilter(altair, prefix)).parTupled.void
