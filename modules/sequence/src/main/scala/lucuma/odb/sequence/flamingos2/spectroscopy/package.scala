// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.spectroscopy

import cats.syntax.option.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.Flamingos2SlitOffsetPreset
import lucuma.core.model.Observation
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as F2
import lucuma.core.model.sequence.flamingos2.defaultSlitTelescopeConfigs
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.ProtoStep

// Definitions shared by the Flamingos 2 spectroscopy sequence generators.

/**
 * A calibration step's exposure time comes from the SmartGcal lookup, so its
 * read mode has to be recomputed rather than inherited from the science steps.
 */
def adjustReadMode(s: ProtoStep[F2]): ProtoStep[F2] =
  val mode = Flamingos2ReadMode.forExposureTime(s.value.exposure)
  s.copy(value = s.value.copy(readMode = mode, reads = mode.readCount))

def definitionError(oid: Observation.Id, msg: String): OdbError =
  OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $msg".some)

def zeroExposureTime(oid: Observation.Id, modeName: String): OdbError =
  definitionError(oid, s"$modeName requires a positive exposure time.")

/**
 * Nod pattern for the telluric standard of a MOS observation.
 */
val MosTelluricTelescopeConfigs: SlitTelescopeConfigs =
  defaultSlitTelescopeConfigs(Flamingos2SlitOffsetPreset.MosTelluric)
