// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

// A daytime calibration is Undefined until it has a science sequence.
object DaytimeCalibrationValidator extends ObservationValidator:

  val sequenceMissing: String =
    "No sequence has been defined."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    if info.hasScienceSteps then ObservationValidationMap.empty
    else ObservationValidationMap.singleton(ObservationValidation.configuration(sequenceMissing))
