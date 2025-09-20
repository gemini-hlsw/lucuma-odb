// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import lucuma.core.enums.GmosRoi
import lucuma.core.math.Wavelength
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.service.CalibrationConfigSubset.*
import munit.FunSuite
    import lucuma.core.enums.*

class CalibrationConfigStrategySuite extends FunSuite:

  def gnConfig(roi: GmosRoi = GmosRoi.CentralSpectrum): Config.GmosNorth =
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

  test("Specphoto doesn't consider ROI"):
    val config1 = gnConfig(GmosRoi.CentralSpectrum)
    val config2 = gnConfig(GmosRoi.FullFrame)

    val matcher = SpecphotoGmosLS

    val subset1 = matcher.extractConfig(config1)
    val subset2 = matcher.extractConfig(config2)

    // Both should produce identical config subsets with CentralSpectrum ROI
    assertEquals(subset1, subset2)

    subset1 match
      case GmosNConfigs(_, _, _, _, _, _, _, _, roi) =>
        assertEquals(roi, GmosRoi.CentralSpectrum)
      case _ => fail("Expected GmosNConfigs")

  test("Twilight uses actual ROI from input"):
    val config1 = gnConfig(GmosRoi.CentralSpectrum)
    val config2 = gnConfig(GmosRoi.FullFrame)

    val matcher = TwilightGmosLS

    val subset1 = matcher.extractConfig(config1)
    val subset2 = matcher.extractConfig(config2)

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

  test("SpectroPhotometric matcher configsMatch ignores ROI differences"):
    val config1 = gnConfig(GmosRoi.CentralSpectrum)
    val config2 = gnConfig(GmosRoi.FullFrame)

    val matcher = SpecphotoGmosLS
    assert(matcher.configsMatch(matcher.extractConfig(config1), matcher.extractConfig(config2)))

  test("Twilight matcher configsMatch preserves ROI differences"):
    val config1 = gnConfig(GmosRoi.CentralSpectrum)
    val config2 = gnConfig(GmosRoi.FullFrame)

    val matcher = TwilightGmosLS
    assert(!matcher.configsMatch(matcher.extractConfig(config1), matcher.extractConfig(config2)))

