// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.GhostBinning
import lucuma.core.enums.GhostReadMode
import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.util.TimeSpan
import lucuma.itc.ItcGhostDetector
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

import scala.concurrent.duration.*

class LegacyITCGhostSpecExpTimeSuite extends CommonITCLegacySuite:

  val wavelengthAt = Wavelength.decimalMicrometers.getOption(2.1).get
  val timeAndCount =
    ExposureTimeMode.TimeAndCountMode(
      TimeSpan.fromSeconds(60).get,
      PosInt.unsafeFrom(10),
      wavelengthAt
    )

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

  override def instrument = ItcInstrumentDetails(
    ObservingMode.SpectroscopyMode.Ghost(
      numSkyMicrolens = 7,
      resolutionMode = GhostResolutionMode.High,
      redDetector = ItcGhostDetector(
        timeAndCount = timeAndCount,
        readMode = GhostReadMode.DefaultRed,
        binning = GhostBinning.OneByOne
      ),
      blueDetector = ItcGhostDetector(
        timeAndCount = timeAndCount,
        readMode = GhostReadMode.DefaultBlue,
        binning = GhostBinning.OneByOne
      )
    )
  )

  test("GHOST spectroscopy S/N".tag(LegacyITCTest)):
    val result = localItc.calculate(baseParams.asJson.noSpaces)
    assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  testConditions("GHOST spectroscopy S/N", baseParams)

  testSEDs("GHOST spectroscopy S/N", baseParams)

  testUserDefinedSED("GHOST spectroscopy S/N", baseParams)

  testBrightnessUnits("GHOST spectroscopy S/N", baseParams)

  testPowerAndBlackbody("GHOST spectroscopy S/N", baseParams)
