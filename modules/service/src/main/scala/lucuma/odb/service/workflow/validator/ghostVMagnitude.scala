// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service
package workflow
package validator

import lucuma.core.enums.Band
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap

// V magnitudes are used by Observe to set the GHOST slit viewing
// camera exposure time, so every target in a GHOST observation needs one.
val ghostVMagnitudeValidator: ObservationValidator = info =>
  val MissingVMagnitude = "Please add a V magnitude."
  if info.observingMode.contains(ObservingModeType.GhostIfu) && info.asterism.exists(!_.sourceProfile.hasBand(Band.V)) then
    ObservationValidationMap.singleton(ObservationValidation.configuration(MissingVMagnitude))
  else ObservationValidationMap.empty