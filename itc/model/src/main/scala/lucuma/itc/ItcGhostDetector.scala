// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc

import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.model.ExposureTimeMode

case class ItcGhostDetector(
  timeAndCount: ExposureTimeMode.TimeAndCountMode, // ITC currently only supports TimeAndCountMode
  readMode:     GhostReadMode,
  binning:      GhostBinning
)
