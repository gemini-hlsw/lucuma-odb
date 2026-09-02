// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * The kind of work a `t_calibration_calc` queue row represents: recalculating
 * calibrations or the target for existing ones.
 */
enum CalibrationWorkType(val tag: String) derives Enumerated:
  case Recalc   extends CalibrationWorkType("recalc")
  case Retarget extends CalibrationWorkType("retarget")
