// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.FieldLens
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservingModeType
import lucuma.odb.data.AltairConfiguration
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*

// Rules governing where an Altair configuration may be attached and what it may say.
object AltairRules:

  val NotGnirsMessage: String =
    "Altair can only be used with GNIRS observing modes."

  val LgsFieldLensMessage: String =
    "Altair LGS modes always use the field lens; fieldLens must be IN or omitted."

  def checkInstrument(mode: ObservingModeType, prefix: String = ""): Result[Unit] =
    if ObservingModeType.toFacility.getOption(mode).exists(_.instrument === Instrument.Gnirs) then Result.unit
    else OdbError.InvalidArgument(s"$prefix$NotGnirsMessage".some).asFailure

  def checkFieldLens(altair: AltairConfiguration, prefix: String = ""): Result[Unit] =
    if altair.mode.usesLaser && altair.explicitFieldLens.contains(FieldLens.Out)
    then OdbError.InvalidArgument(s"$prefix$LgsFieldLensMessage".some).asFailure
    else Result.unit
