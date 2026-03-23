// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

/**
 * Unit test for igrins 2 integration time calculation
 */
class LegacyITCIgrins2SpecSignalToNoiseSuite extends CommonITCLegacySuite:

  val wavelengthAt = Wavelength.decimalMicrometers.getOption(2.1).get

  override def obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
        sigma = 100,
        wavelengthAt = wavelengthAt,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
    analysisMethod = lsAnalysisMethod
  )

  override def instrument = ItcInstrumentDetails(ObservingMode.SpectroscopyMode.Igrins2())

  test("IGRINS-2 spectroscopy integration time".tag(LegacyITCTest)):
    val result = localItc.calculateIntegrationTime(baseParams.asJson.noSpaces)
    assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  testConditions("IGRINS-2 spectroscopy integration time", baseParams)

  testSEDs("IGRINS-2 spectroscopy integration time", baseParams)

  testUserDefinedSED("IGRINS-2 spectroscopy integration time", baseParams)

  testBrightnessUnits("IGRINS-2 spectroscopy integration time", baseParams)

  testPowerAndBlackbody("IGRINS-2 spectroscopy integration time", baseParams)
