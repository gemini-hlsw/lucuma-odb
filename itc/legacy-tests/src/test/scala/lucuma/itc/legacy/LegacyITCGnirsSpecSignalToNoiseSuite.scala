// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import eu.timepit.refined.types.numeric.PosInt
import io.circe.syntax.*
import lucuma.core.enums.GnirsCamera
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

/**
 * Unit test for GNIRS integration time (signal-to-noise) calculation. Mirrors
 * [[LegacyITCGnirsSpecExpTimeSuite]] but exercises the integration-time direction (given a desired
 * S/N, the legacy ITC computes exposure time and count).
 */
class LegacyITCGnirsSpecSignalToNoiseSuite extends CommonITCLegacySuite:

  val centralWavelength = Wavelength.decimalMicrometers.getOption(2.2).get
  val wavelengthAt      = Wavelength.decimalMicrometers.getOption(2.1).get

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

  val gnirs = ObservingMode.SpectroscopyMode.GnirsSpectroscopy(
    centralWavelength = centralWavelength,
    grating = GnirsGrating.D32,
    filter = GnirsFilter.Order3,
    camera = GnirsCamera.ShortBlue,
    prism = GnirsPrism.Mirror,
    readMode = GnirsReadMode.Bright,
    fpu = GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
    wellDepth = GnirsWellDepth.Shallow,
    coadds = PosInt.unsafeFrom(1),
    portDisposition = PortDisposition.Bottom
  )

  override def instrument = ItcInstrumentDetails(gnirs)

  // Strict regression test for the GNIRS S/N wavelength-unit bug. The OCS GnirsRecipe
  // multiplies SpectroscopyIntegrationTime.wavelengthAt by 1000 (it expects microns),
  // but the JSON path sends nm. Before the OCS-side fix (adding paramsInMicrons for
  // GnirsParameters in ItcServiceImpl) this call returned "Wavelength = ... nm is out
  // of range". Unlike the per-parameter tests below this asserts containsValidResults
  // directly — no allowedErrors fallback (which would silently absorb the bug).
  test("gnirs signal-to-noise base config yields valid results".tag(LegacyITCTest)):
    val result = localItc.calculate(baseParams.asJson.noSpaces)
    assertIOBoolean(result.map(_.fold(_ => false, containsValidResults)))

  // Exercises the IFU path through the real OCS jars: slitWidth encodes to LR_IFU,
  // crossDispersed to NO, and the analysis method is "sum of 2x2 elements at the
  // center" with a single sky fibre (the production default). LR-IFU requires the
  // 0.15"/pix (Short) camera.
  test("gnirs IFU yields valid results".tag(LegacyITCTest)):
    val ifuMode     = gnirs.copy(
      fpu = GnirsFpu.Spectroscopy.Ifu(GnirsFpuIfu.LowResolution),
      camera = GnirsCamera.ShortBlue
    )
    val ifuAnalysis = ItcObservationDetails.AnalysisMethod.Ifu.Summed(
      skyFibres = 1,
      numX = 2,
      numY = 2,
      centerX = 0.0,
      centerY = 0.0
    )
    val result      = localItc.calculate:
      bodyConf(sourceDefinition, obs, ifuMode, ifuAnalysis).asJson.noSpaces
    assertIOBoolean(result.map(_.fold(_ => false, containsValidResults)))

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
        bodyConf(sourceDefinition, obs, gnirs.copy(fpu = GnirsFpu.Spectroscopy.Slit(s))).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gnirs well depth".tag(LegacyITCTest)):
    Enumerated[GnirsWellDepth].all.foreach: w =>
      val result = localItc.calculate:
        bodyConf(sourceDefinition, obs, gnirs.copy(wellDepth = w)).asJson.noSpaces
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  testConditions("GNIRS spectroscopy integration time", baseParams)

  testSEDs("GNIRS spectroscopy integration time", baseParams)

  testUserDefinedSED("GNIRS spectroscopy integration time", baseParams)

  testBrightnessUnits("GNIRS spectroscopy integration time", baseParams)

  testPowerAndBlackbody("GNIRS spectroscopy integration time", baseParams)
