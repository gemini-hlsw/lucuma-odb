// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

import scala.concurrent.duration.*
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsWellDepth

/**
 * Unit test for GNIRS exposure time calculation
 */
class LegacyITCGnirsSpecExpTimeSuite extends CommonITCLegacySuite:

  val centralWavelength = Wavelength.decimalMicrometers.getOption(2.2).get
  val wavelengthAt      = Wavelength.decimalMicrometers.getOption(2.1).get

  override def obs = ItcObservationDetails(
    calculationMethod = ItcObservationDetails.CalculationMethod.S2NMethod.SpectroscopyS2N(
      exposureCount = 30,
      exposureDuration = 120.seconds,
      wavelengthAt = wavelengthAt,
      coadds = 1.some,
      sourceFraction = 1.0,
      ditherOffset = Angle.fromDoubleArcseconds(5)
    ),
    analysisMethod = ItcObservationDetails.AnalysisMethod.Aperture.Auto(1)
  )

  override def instrument = ItcInstrumentDetails(
    ObservingMode.SpectroscopyMode.GnirsLongSlit(
      centralWavelength = centralWavelength,
      grating = GnirsGrating.D32,
      filter = GnirsFilter.Order3,
      camera = GnirsCamera.ShortBlue,
      prism = GnirsPrism.Mirror,
      readMode = GnirsReadMode.Bright,
      slitWidth = GnirsFpuSlit.LongSlit_0_30,
      wellDepth = GnirsWellDepth.Shallow,
      portDisposition = PortDisposition.Bottom
    )
  )

  test("GNIRS spectroscopy S/N".tag(LegacyITCTest)):
    println(s"INVOKING: ${baseParams.asJson.spaces2}")
    // TODO TRY OUT ALL PARAMETER COMBINATIONS LIKE IN OTHER INSTRUMENTS
    val result = localItc
      .calculate(baseParams.asJson.noSpaces)
      .flatTap(r => cats.effect.IO.println("**** RESULT ****\n" + r))
    assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  // testConditions("GNIRS spectroscopy S/N", baseParams)

  // testSEDs("GNIRS spectroscopy S/N", baseParams)

  // testUserDefinedSED("GNIRS spectroscopy S/N", baseParams)

  // testBrightnessUnits("GNIRS spectroscopy S/N", baseParams)

  // testPowerAndBlackbody("GNIRS spectroscopy S/N", baseParams)
