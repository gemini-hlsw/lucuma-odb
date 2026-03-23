// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

import scala.concurrent.duration.*

/**
 * Unit test for igrins 2 exposure time calculation
 */
class LegacyITCIgrins2SpecExpTimeSuite extends CommonITCLegacySuite:

  val wavelengthAt = Wavelength.decimalMicrometers.getOption(2.1).get

  override def obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
      exposureCount = 10,
      exposureDuration = 60.seconds,
      wavelengthAt = wavelengthAt,
      coadds = None,
      sourceFraction = 1.0,
      ditherOffset = Angle.Angle0
    ),
    analysisMethod = lsAnalysisMethod
  )

  override def instrument = ItcInstrumentDetails(ObservingMode.SpectroscopyMode.Igrins2())

  test("IGRINS-2 spectroscopy S/N".tag(LegacyITCTest)):
    val result = localItc.calculateIntegrationTime(baseParams.asJson.noSpaces)
    assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  testConditions("IGRINS-2 spectroscopy S/N", baseParams)

  testSEDs("IGRINS-2 spectroscopy S/N", baseParams)

  testUserDefinedSED("IGRINS-2 spectroscopy S/N", baseParams)

  testBrightnessUnits("IGRINS-2 spectroscopy S/N", baseParams)

  testPowerAndBlackbody("IGRINS-2 spectroscopy S/N", baseParams)
