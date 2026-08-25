// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy

import cats.implicits.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ToBeDefined
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
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
      GmosNorthFpuParam(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_5_00)),
      none,
      GmosCcdMode(
        GmosXBinning.One,
        GmosYBinning.One,
        GmosAmpCount.Twelve,
        GmosAmpGain.High,
        GmosAmpReadMode.Fast
      ).some,
      GmosRoi.FullFrame.some,
      PortDisposition.Side,
      none
    )
  )

  // Testing observing conditions
  testConditions("GMOS spectroscopy S/N", baseParams)

  val gnConf = ObservingMode.SpectroscopyMode.GmosNorth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosNorthGrating.B1200_G5301,
    GmosNorthFpuParam(GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_1_00)),
    none,
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some,
    PortDisposition.Side,
    none
  )

  test("gmos north grating".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosNorthGrating].all): d =>
      localItc
        .calculate(
          bodyConf(sourceDefinition, obs, gnConf.copy(disperser = d)).asJson.noSpaces
        )

  test("gmos north filter".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosNorthFilter].all): f =>
      localItc
        .calculate(
          bodyConf(sourceDefinition, obs, gnConf.copy(filter = f.some)).asJson.noSpaces
        )

  test("gmos north fpu".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosNorthFpu].all): f =>
      localItc
        .calculate(
          bodyConf(
            sourceDefinition,
            obs,
            gnConf.copy(fpu = GmosNorthFpuParam(GmosFpuMask.Builtin(f))),
            // Exercise the analysis method production would pick, IFU included.
            analysis = gnConf.copy(fpu = GmosNorthFpuParam(GmosFpuMask.Builtin(f))).analysisMethod
          ).asJson.noSpaces
        )

  test("gmos north mos custom slit width".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosCustomSlitWidth].all): w =>
      localItc
        .calculate(
          bodyConf(
            sourceDefinition,
            obs,
            gnConf.copy(fpu = GmosNorthFpuParam(GmosFpuMask.Custom(ToBeDefined, w)))
          ).asJson.noSpaces
        )

  val gsConf = ObservingMode.SpectroscopyMode.GmosSouth(
    Wavelength.decimalNanometers.getOption(600).get,
    GmosSouthGrating.B1200_G5321,
    GmosSouthFpuParam(GmosFpuMask.Builtin(GmosSouthFpu.LongSlit_1_00)),
    none,
    GmosCcdMode(GmosXBinning.One,
                GmosYBinning.One,
                GmosAmpCount.Twelve,
                GmosAmpGain.High,
                GmosAmpReadMode.Fast
    ).some,
    GmosRoi.FullFrame.some,
    PortDisposition.Side,
    none
  )

  test("gmos south grating".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosSouthGrating].all): d =>
      localItc
        .calculate(
          bodyConf(sourceDefinition, obs, gsConf.copy(disperser = d)).asJson.noSpaces
        )

  test("gmos south filter".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosSouthFilter].all): f =>
      localItc
        .calculate(
          bodyConf(sourceDefinition, obs, gsConf.copy(filter = f.some)).asJson.noSpaces
        )

  test("gmos south fpu".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosSouthFpu].all): f =>
      localItc
        .calculate(
          bodyConf(
            sourceDefinition,
            obs,
            gsConf.copy(fpu = GmosSouthFpuParam(GmosFpuMask.Builtin(f))),
            analysis = gsConf.copy(fpu = GmosSouthFpuParam(GmosFpuMask.Builtin(f))).analysisMethod
          ).asJson.noSpaces
        )

  test("gmos south mos custom slit width".tag(LegacyITCTest)):
    assertAllValid(Enumerated[GmosCustomSlitWidth].all): w =>
      localItc
        .calculate(
          bodyConf(
            sourceDefinition,
            obs,
            gsConf.copy(fpu = GmosSouthFpuParam(GmosFpuMask.Custom(ToBeDefined, w)))
          ).asJson.noSpaces
        )

  // Testing various SEDs
  testSEDs("GMOS spectroscopy S/N", baseParams)

  // Testing user defined SED
  testUserDefinedSED("GMOS spectroscopy S/N", baseParams)

  // Testing brightness units
  testBrightnessUnits("GMOS spectroscopy S/N", baseParams)

  // Testing power law and blackbody
  testPowerAndBlackbody("GMOS spectroscopy S/N", baseParams)
