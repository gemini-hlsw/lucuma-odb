// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2.spectroscopy

import lucuma.core.enums.Flamingos2TelluricOffsetPreset
import lucuma.core.model.Observation
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.sequence.flamingos2.defaultTelluricTelescopeConfigs
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.util.definitionError

// Definitions shared by the Flamingos 2 spectroscopy sequence generators.

def zeroExposureTime(oid: Observation.Id, modeName: String): OdbError =
  definitionError(oid, s"$modeName requires a positive exposure time.")

/**
 * Nod pattern for the telluric standard of a MOS observation.
 */
val MosTelluricTelescopeConfigs: SlitTelescopeConfigs =
  defaultTelluricTelescopeConfigs(Flamingos2TelluricOffsetPreset.MosTelluric)
