// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.order.*
import lucuma.core.math.Wavelength
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan

/**
 * Nighttime calibration cadence shared by the infrared spectrographs (Flamingos
 * 2 and GNIRS): one set of flats, arcs and a telluric standard per period of
 * science.  The period tightens in the thermal infrared.
 */
object InfraredCalibration:

  /** Wavelength from which the thermal infrared cadence applies. */
  val ThermalBoundary: Wavelength =
    Wavelength.unsafeFromIntPicometers(2_600_000)

  /** Cadence below `ThermalBoundary`. */
  val NearInfraredPeriod: TimeSpan =
    90.minuteTimeSpan

  /** Cadence at or above `ThermalBoundary`. */
  val ThermalInfraredPeriod: TimeSpan =
    1.hourTimeSpan

  def period(wavelength: Wavelength): TimeSpan =
    if wavelength < ThermalBoundary then NearInfraredPeriod else ThermalInfraredPeriod
