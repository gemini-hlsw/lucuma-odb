// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.ObservingModeType
import lucuma.core.math.Wavelength
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.service.CalibrationConfigSubset.*
import munit.CatsEffectSuite

class CalibrationConfigStrategySuite extends CatsEffectSuite {

  // Helper to create a basic GMOS North longslit config
  def gmosNorthConfig(roi: GmosRoi = GmosRoi.CentralSpectrum): Config.GmosNorth = {
    import lucuma.core.enums.*
    Config.GmosNorth(
      grating = GmosNorthGrating.B1200_G5301,
      filter = GmosNorthFilter.GPrime.some,
      fpu = GmosNorthFpu.LongSlit_1_00,
      common = Config.Common(
        centralWavelength = Wavelength.fromIntNanometers(500).get,
        defaultXBin = GmosXBinning.One,
        explicitXBin = None,
        defaultYBin = GmosYBinning.One,
        explicitYBin = None,
        explicitAmpReadMode = None,
        explicitAmpGain = None,
        explicitRoi = roi.some,
        explicitWavelengthDithers = None,
        explicitSpatialOffsets = None
      )
    )
  }

  test("SpectroPhotometric strategy uses CentralSpectrum ROI regardless of input") {
    val config1 = gmosNorthConfig(GmosRoi.CentralSpectrum)
    val config2 = gmosNorthConfig(GmosRoi.FullFrame)

    val strategy = SpecphotoGmosLS

    val subset1 = strategy.extractConfig(config1)
    val subset2 = strategy.extractConfig(config2)

    // Both should produce identical config subsets with CentralSpectrum ROI
    assertEquals(subset1, subset2)

    subset1 match {
      case GmosNConfigs(_, _, _, _, _, _, _, _, roi) =>
        assertEquals(roi, GmosRoi.CentralSpectrum)
      case _ => fail("Expected GmosNConfigs")
    }
  }

  test("Twilight strategy uses actual ROI from input") {
    val config1 = gmosNorthConfig(GmosRoi.CentralSpectrum)
    val config2 = gmosNorthConfig(GmosRoi.FullFrame)

    val strategy = TwilightGmosLS

    val subset1 = strategy.extractConfig(config1)
    val subset2 = strategy.extractConfig(config2)

    // Should produce different config subsets due to ROI difference
    assertNotEquals(subset1, subset2)

    subset1 match {
      case GmosNConfigs(_, _, _, _, _, _, _, _, roi) =>
        assertEquals(roi, GmosRoi.CentralSpectrum)
      case _ => fail("Expected GmosNConfigs")
    }

    subset2 match {
      case GmosNConfigs(_, _, _, _, _, _, _, _, roi) =>
        assertEquals(roi, GmosRoi.FullFrame)
      case _ => fail("Expected GmosNConfigs")
    }
  }

  test("Registry returns correct strategies") {
    val spectroStrategy = CalibrationConfigStrategy.getStrategy(
      ObservingModeType.GmosNorthLongSlit,
      CalibrationRole.SpectroPhotometric
    )
    val twilightStrategy = CalibrationConfigStrategy.getStrategy(
      ObservingModeType.GmosNorthLongSlit,
      CalibrationRole.Twilight
    )

    assert(spectroStrategy.isDefined)
    assert(twilightStrategy.isDefined)

    assertEquals(spectroStrategy.get.calibrationType, CalibrationRole.SpectroPhotometric)
    assertEquals(twilightStrategy.get.calibrationType, CalibrationRole.Twilight)
  }

  test("SpectroPhotometric strategy configsMatch ignores ROI differences") {
    val config1 = gmosNorthConfig(GmosRoi.CentralSpectrum)
    val config2 = gmosNorthConfig(GmosRoi.FullFrame)

    val strategy = SpecphotoGmosLS
    val subset1 = strategy.extractConfig(config1)
    val subset2 = strategy.extractConfig(config2)

    // Configs should match despite different source ROIs
    assert(strategy.configsMatch(subset1, subset2))
  }

  test("Twilight strategy configsMatch preserves ROI differences") {
    val config1 = gmosNorthConfig(GmosRoi.CentralSpectrum)
    val config2 = gmosNorthConfig(GmosRoi.FullFrame)

    val strategy = TwilightGmosLS
    val subset1 = strategy.extractConfig(config1)
    val subset2 = strategy.extractConfig(config2)

    // Configs should NOT match due to ROI differences
    assert(!strategy.configsMatch(subset1, subset2))
  }

  test("Registry getStrategyForComparison works correctly") {
    val config = SpecphotoGmosLS.extractConfig(gmosNorthConfig())

    val spectroStrategy = CalibrationConfigStrategy.getStrategyForComparison(
      config,
      CalibrationRole.SpectroPhotometric
    )
    val twilightStrategy = CalibrationConfigStrategy.getStrategyForComparison(
      config,
      CalibrationRole.Twilight
    )

    assert(spectroStrategy.isDefined)
    assert(twilightStrategy.isDefined)
    assertEquals(spectroStrategy.get.calibrationType, CalibrationRole.SpectroPhotometric)
    assertEquals(twilightStrategy.get.calibrationType, CalibrationRole.Twilight)
  }

  test("Registry returns None for unsupported combinations") {
    val strategy = CalibrationConfigStrategy.getStrategy(
      ObservingModeType.GmosNorthImaging,
      CalibrationRole.SpectroPhotometric
    )

    assertEquals(strategy, None)
  }

  test("Registry lists supported calibration types") {
    val supportedTypes = CalibrationConfigStrategy.getSupportedCalibrationTypes(
      ObservingModeType.GmosNorthLongSlit
    )

    assertEquals(supportedTypes.size, 2)
    assert(supportedTypes.contains(CalibrationRole.SpectroPhotometric))
    assert(supportedTypes.contains(CalibrationRole.Twilight))
  }

  test("Strategies produce configurations with all required fields") {
    val config = gmosNorthConfig(GmosRoi.FullFrame)

    val spectroSubset = SpecphotoGmosLS.extractConfig(config)
    val twilightSubset = TwilightGmosLS.extractConfig(config)

    // Both should be valid GmosNConfigs with proper field values
    spectroSubset match {
      case GmosNConfigs(grating, filter, fpu, wavelength, xBin, yBin, ampReadMode, ampGain, roi) =>
        assertEquals(grating.tag, "B1200_G5301")
        assertEquals(fpu.tag, "LongSlit_1_00")
        assertEquals(wavelength.toPicometers.value.value, 500000)
        assertEquals(roi, GmosRoi.CentralSpectrum)
      case _ => fail("Expected GmosNConfigs")
    }

    twilightSubset match {
      case GmosNConfigs(grating, filter, fpu, wavelength, xBin, yBin, ampReadMode, ampGain, roi) =>
        assertEquals(grating.tag, "B1200_G5301")
        assertEquals(fpu.tag, "LongSlit_1_00")
        assertEquals(wavelength.toPicometers.value.value, 500000)
        assertEquals(roi, GmosRoi.FullFrame)
      case _ => fail("Expected GmosNConfigs")
    }
  }
}
