// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos

import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

// Definitions that are shared across GMOS modes.
val MinAcquisitionExposureTime: TimeSpan =   1.secondTimeSpan
val MaxAcquisitionExposureTime: TimeSpan = 180.secondTimeSpan