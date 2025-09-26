// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

import scala.concurrent.duration.*

/**
 * This is a unit test for Flamingos2 imaging mode in the legacy ITC, ensuring all possible
 * combinations of parameters can be parsed. The ITC may still return an error but we want to ensure
 * it can parse the values.
 */
class LegacyITCFlamingos2ImgExpTimeSuite extends LegacyITCFlamingos2Suite:

  lazy val analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(5)

  override def obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.ImagingS2N(
      exposureDuration = 10.seconds,
      exposureCount = 5,
      coadds = None,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = analysisMethod
  )

  lazy val f2 = ObservingMode.ImagingMode.Flamingos2(Flamingos2Filter.J)

  override def instrument = ItcInstrumentDetails(f2)

  override def title = "Flamingos2 Imaging TxC"

  def observingModeWithFilter(f: Flamingos2Filter): ObservingMode =
    f2.copy(filter = f)

  def observingModeWithFpu(f: Flamingos2Fpu): ObservingMode =
    f2
