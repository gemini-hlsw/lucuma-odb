// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gnirs

import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

// Definitions that are shared across GNIRS modes.
val MinAcquisitionExposureTime: TimeSpan = 100.msTimeSpan  // 0.1 s; actually determined by the read mode, but this is a reasonable lower bound for all modes.
val MaxAcquisitionExposureTime: TimeSpan = 60.secondTimeSpan
