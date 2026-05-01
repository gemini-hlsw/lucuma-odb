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
import lucuma.core.util.Enumerated

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

  val gnirs = ObservingMode.SpectroscopyMode.GnirsLongSlit(
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

  override def instrument = ItcInstrumentDetails(gnirs)

  test("gnirs grating".tag(LegacyITCTest)):
    Enumerated[GnirsGrating].all.foreach: g =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(grating = g)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs filter".tag(LegacyITCTest)):
    Enumerated[GnirsFilter].all.foreach: f =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(filter = f)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs camera".tag(LegacyITCTest)):
    Enumerated[GnirsCamera].all.foreach: c =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(camera = c)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs prism".tag(LegacyITCTest)):
    Enumerated[GnirsPrism].all.foreach: p =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(prism = p)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs read mode".tag(LegacyITCTest)):
    Enumerated[GnirsReadMode].all.foreach: r =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(readMode = r)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs slit width".tag(LegacyITCTest)):
    Enumerated[GnirsFpuSlit].all.foreach: s =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(slitWidth = s)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs well depth".tag(LegacyITCTest)):
    Enumerated[GnirsWellDepth].all.foreach: w =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(wellDepth = w)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  testConditions("GNIRS spectroscopy S/N", baseParams)

  testSEDs("GNIRS spectroscopy S/N", baseParams)

  testUserDefinedSED("GNIRS spectroscopy S/N", baseParams)

  testBrightnessUnits("GNIRS spectroscopy S/N", baseParams)

  testPowerAndBlackbody("GNIRS spectroscopy S/N", baseParams)
