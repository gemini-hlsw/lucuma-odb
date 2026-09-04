// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Timestamp

/**
 * A claimed row of the calibration work queue (`t_calibration_calc`): a
 * science observation whose program's calibrations may need recalculation, or
 * a calibration observation whose target may need re-picking for a new
 * observation time.
 */
final case class PendingRecalc(
  programId:        Program.Id,
  observationId:    Observation.Id,
  lastInvalidation: Timestamp,
  workType:         CalibrationWorkType
)
