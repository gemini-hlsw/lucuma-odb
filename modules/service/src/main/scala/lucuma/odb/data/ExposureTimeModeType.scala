// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

enum ExposureTimeModeType(val tag: String) derives Enumerated:
  case SignalToNoiseMode extends ExposureTimeModeType("signal_to_noise")
  case TimeAndCountMode  extends ExposureTimeModeType("time_and_count")