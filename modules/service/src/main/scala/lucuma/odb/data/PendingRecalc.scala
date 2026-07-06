// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.Timestamp

/**
 * A claimed row of the calibration-recalculation queue (`t_calibration_calc`):
 * a science observation whose program's calibrations may need recalculation.
 */
final case class PendingRecalc(
  programId:        Program.Id,
  observationId:    Observation.Id,
  lastInvalidation: Timestamp
)
