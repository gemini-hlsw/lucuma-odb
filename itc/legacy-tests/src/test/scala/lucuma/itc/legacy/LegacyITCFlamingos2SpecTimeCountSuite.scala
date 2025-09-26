// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

import scala.concurrent.duration.*

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCFlamingos2SpecTimeAndCountSuite extends LegacyITCFlamingos2Suite:
  override def analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)

  override def obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
      exposureCount = 10,
      exposureDuration = 3.seconds,
      wavelengthAt = Wavelength.decimalNanometers.getOption(1200).get,
      coadds = None,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = lsAnalysisMethod
  )

  lazy val f2 =
    ObservingMode.SpectroscopyMode.Flamingos2(
      Flamingos2Disperser.R3000,
      Flamingos2Filter.J,
      Flamingos2Fpu.LongSlit2
    )

  override def instrument = ItcInstrumentDetails(f2)

  override def title = "Flamingos2 Spectroscopy S/N"

  def observingModeWithFilter(f: Flamingos2Filter): ObservingMode =
    val d = f match
      case Flamingos2Filter.J | Flamingos2Filter.H | Flamingos2Filter.JH | Flamingos2Filter.HK =>
        Flamingos2Disperser.R1200JH
      case _                                                                                   => Flamingos2Disperser.R3000
    f2.copy(filter = f, disperser = d)

  def observingModeWithFpu(f: Flamingos2Fpu): ObservingMode =
    f2.copy(fpu = f)
