// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.order.*
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * Nighttime calibration cadence shared by the infrared spectrographs: one
 * calibration set per interval of science, shorter in the thermal infrared.
 */
object InfraredCalibration:

  /** Wavelength from which infrared calibrations repeat hourly rather than every 90 minutes. 2.6 microns */
  val LongWavelengthCutoff: Wavelength = Wavelength.unsafeFromIntPicometers(2_600_000)

  val ShortWavelengthSetInterval: TimeSpan = 90.minTimeSpan

  val LongWavelengthSetInterval: TimeSpan = 60.minTimeSpan

  def calibrationSetInterval(wavelength: Wavelength): TimeSpan =
    if wavelength < LongWavelengthCutoff then ShortWavelengthSetInterval else LongWavelengthSetInterval
