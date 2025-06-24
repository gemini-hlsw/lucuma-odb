// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.flamingos2

import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

// Definitions that are shared across F2 modes.
val MinAcquisitionExposureTime: TimeSpan = 2.secondTimeSpan
val MaxAcquisitionExposureTime: TimeSpan = TimeSpan.Max  // TBD