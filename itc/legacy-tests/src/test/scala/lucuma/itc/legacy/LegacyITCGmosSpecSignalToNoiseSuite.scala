// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.implicits.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.itc.legacy.codecs.given
import lucuma.itc.service.GmosNorthFpuParam
import lucuma.itc.service.GmosSouthFpuParam
import lucuma.itc.service.ItcObservationDetails
import lucuma.itc.service.ObservingMode

/**
 * This is a unit test mostly to ensure all possible combination of params can be parsed by the
 * legacy ITC (Note that the ITC may still return an error but we want to ensure it can parse the
 * values
 */
class LegacyITCGmosSpecSignalToNoiseSuite extends CommonITCLegacySuite:

  override val obs = ItcObservationDetails(
    calculationMethod =
      ItcObservationDetails.CalculationMethod.IntegrationTimeMethod.SpectroscopyIntegrationTime(
        sigma = 100,
        wavelengthAt = Wavelength.decimalNanometers.getOption(610).get,
        coadds = None,
        sourceFraction = 1.0,
        ditherOffset = Angle.Angle0
      ),
    analysisMethod = lsAnalysisMethod
  )

  override val instrument = ItcInstrumentDetails(
    ObservingMode.SpectroscopyMode.GmosNorth(
      Wavelength.decimalNanometers.getOption(600).get,
      GmosNorthGrating.B1200_G5301,
      GmosNorthFpuParam(GmosNorthFpu.LongSlit_5_00),
      none,
      GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.High,
        GmosAmpReadMode.Fast
      ).some,
      GmosRoi.FullFrame.some
    )
  )

  // Testing observing conditions
  testConditions("GMOS spectroscopy S/N", baseParams)

  val gnConf = ObservingMode.SpectroscopyMode.GmosNorth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosNorthGrating.B1200_G5301,
    GmosNorthFpuParam(GmosNorthFpu.LongSlit_1_00),
    none,
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some
  )

  test("gmos north grating".tag(LegacyITCTest)):
    Enumerated[GmosNorthGrating].all.foreach: d =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, gnConf.copy(disperser = d)).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gmos north filter".tag(LegacyITCTest)):
    Enumerated[GmosNorthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, gnConf.copy(filter = f.some)).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gmos north fpu".tag(LegacyITCTest)):
    Enumerated[GmosNorthFpu].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition,
                   obs,
                   gnConf.copy(fpu = GmosNorthFpuParam(f)),
                   analysis = if (f.isIFU) ifuAnalysisMethod else lsAnalysisMethod
          ).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  val gsConf = ObservingMode.SpectroscopyMode.GmosSouth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosSouthGrating.B1200_G5321,
    GmosSouthFpuParam(GmosSouthFpu.LongSlit_1_00),
    none,
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some
  )

  test("gmos south grating".tag(LegacyITCTest)):
    Enumerated[GmosSouthGrating].all.foreach: d =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, gsConf.copy(disperser = d)).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gmos south filter".tag(LegacyITCTest)):
    Enumerated[GmosSouthFilter].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition, obs, gsConf.copy(filter = f.some)).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  test("gmos south fpu".tag(LegacyITCTest)):
    Enumerated[GmosSouthFpu].all.foreach: f =>
      val result = localItc
        .calculateIntegrationTime(
          bodyConf(sourceDefinition,
                   obs,
                   gsConf.copy(fpu = GmosSouthFpuParam(f)),
                   analysis = if (f.isIFU) ifuAnalysisMethod else lsAnalysisMethod
          ).asJson.noSpaces
        )
      assertIOBoolean(result.map(_.fold(allowedErrors, containsValidResults)))

  // Testing various SEDs
  testSEDs("GMOS spectroscopy S/N", baseParams)

  // Testing user defined SED
  testUserDefinedSED("GMOS spectroscopy S/N", baseParams)

  // Testing brightness units
  testBrightnessUnits("GMOS spectroscopy S/N", baseParams)

  // Testing power law and blackbody
  testPowerAndBlackbody("GMOS spectroscopy S/N", baseParams)
