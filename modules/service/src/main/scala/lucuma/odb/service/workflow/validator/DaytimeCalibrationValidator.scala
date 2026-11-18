// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service.workflow.validator

import lucuma.core.model.ObservationValidation
import lucuma.odb.data.ObservationValidationMap
import lucuma.odb.service.workflow.ObservationValidationInfo
import lucuma.odb.service.workflow.ObservationValidator

// A daytime calibration is Undefined until it has a science sequence, and
// while a calibration set's custom mask has no usable attachment.
object DaytimeCalibrationValidator extends ObservationValidator:

  val sequenceMissing: String =
    "No sequence has been defined."

  val maskMissing: String =
    "A calibration set uses a custom mask without a mask attachment. Choose a mask."

  val maskMismatch: String =
    "A calibration set uses an attachment that is no longer a MOS mask for its instrument. Choose another mask."

  def apply(info: ObservationValidationInfo): ObservationValidationMap =
    ObservationValidationMap.fromList:
      List(
        Option.unless(info.hasSequenceSource)(sequenceMissing),
        Option.when(info.daytimeMaskMissing)(maskMissing),
        Option.when(info.daytimeMaskMismatch)(maskMismatch)
      ).flatten.map(ObservationValidation.configuration(_))
