// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.order.*
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * How much science one nighttime calibration set covers on the infrared
 * spectrographs: 90 minutes, or an hour from the long wavelength cutoff.
 */
object CalibrationSetInterval:

  /** Wavelength from which infrared calibrations repeat hourly rather than every 90 minutes. 2.6 microns */
  val LongWavelengthCutoff: Wavelength = Wavelength.unsafeFromIntPicometers(2_600_000)

  val ShortWavelength: TimeSpan = 90.minTimeSpan

  val LongWavelength: TimeSpan = 60.minTimeSpan

  def forWavelength(wavelength: Wavelength): TimeSpan =
    if wavelength < LongWavelengthCutoff then ShortWavelength else LongWavelength

  /** Started intervals in `scienceTime`: ceil(scienceTime / interval), zero for no science. */
  def intervalsIn(interval: TimeSpan, scienceTime: TimeSpan): Int =
    val i = interval.toMicroseconds
    ((scienceTime.toMicroseconds + i - 1) / i).toInt
