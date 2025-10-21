// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.*
import lucuma.core.enums.GmosRoi
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.syntax.timespan.*
import lucuma.core.util.arb.ArbEnumerated.given
import lucuma.odb.sequence.gmos.longslit.Config
import lucuma.odb.service.CalibrationConfigMatcher.UnknownConfig
import lucuma.odb.service.CalibrationConfigSubset.*
import lucuma.odb.service.arb.ArbCalibrationConfigSubset.given
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean

class CalibrationMatchersSuite extends ScalaCheckSuite:

  def gnConfig(roi: GmosRoi = GmosRoi.CentralSpectrum): Config.GmosNorth =
    val w = Wavelength.fromIntNanometers(500).get
    val e = ExposureTimeMode.TimeAndCountMode(10.secondTimeSpan, PosInt.unsafeFrom(1), w)
    Config.GmosNorth(
      grating = GmosNorthGrating.B1200_G5301,
      filter = GmosNorthFilter.GPrime.some,
      defaultAcquisitionFilter = GmosNorthFilter.GPrime,
      explicitAcquisitionFilter = none,
      fpu = GmosNorthFpu.LongSlit_1_00,
      common = Config.Common(
        centralWavelength = w,
        acquisitionExposureTimeMode = e,
        scienceExposureTimeMode = e,
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

  test("Flamingos2LS has a config set for Telluric"):
    val config = arbitrary[Flamingos2Configs].sample.get
    assertEquals(UnknownConfig, CalibrationConfigMatcher.matcherFor(config, CalibrationRole.SpectroPhotometric))
    assertEquals(UnknownConfig, CalibrationConfigMatcher.matcherFor(config, CalibrationRole.Twilight))
    assertEquals(Flamingos2LS, CalibrationConfigMatcher.matcherFor(config, CalibrationRole.Telluric))

  property("SpecphotoGmosLS normalize is idempotent"):
    forAll: (config: GmosNConfigs) =>
      val matcher = SpecphotoGmosLS
      val normalized = matcher.normalize(config)
      val doubleNormalized = matcher.normalize(normalized)
      normalized == doubleNormalized

  property("SpecphotoGmosLS normalize is idempotent for GMOS South"):
    forAll: (config: GmosSConfigs) =>
      val matcher = SpecphotoGmosLS
      val normalized = matcher.normalize(config)
      val doubleNormalized = matcher.normalize(normalized)
      normalized == doubleNormalized

  property("Flamingos2LS normalize is idempotent for Flamingos2"):
    forAll: (config: Flamingos2Configs) =>
      val matcher = Flamingos2LS
      val normalized = matcher.normalize(config)
      val doubleNormalized = matcher.normalize(normalized)
      normalized == doubleNormalized

  test("Normalization + diff workflow simulates calculateConfigurationsPerRole"):
    val base1 = arbitrary[GmosNConfigs].sample.get
    val base2 = arbitrary[GmosNConfigs].sample.get.copy(grating = GmosNorthGrating.R831_G5302)

    val matcher = SpecphotoGmosLS

    // different ROIs
    val sciConfigs = List(
      base1.copy(roi = GmosRoi.FullFrame),
      base1.copy(roi = GmosRoi.CentralSpectrum),
      base2.copy(roi = GmosRoi.FullFrame)
    )

    val calibConfigs = List(base1.copy(roi = GmosRoi.CentralSpectrum))

    val normalizedSci = sciConfigs.map(matcher.normalize).distinct
    val normalizedCalib = calibConfigs.map(matcher.normalize).distinct

    // Calculate new configs needed
    val newConfigs = normalizedSci.diff(normalizedCalib)

    // Should only need base2 calibration (base1 already exists)
    assertEquals(newConfigs.size, 1)
    newConfigs.headOption match
      case Some(gn: GmosNConfigs) => assertEquals(gn.grating, base2.grating)
      case _                      => fail("Expected GmosNConfigs")

  property("Flamingos2LS matches identical configs"):
    forAll: (config: Flamingos2Configs) =>
      val matcher = Flamingos2LS
      matcher.configsMatch(config, config)

  property("Flamingos2LS does not match configs with different disperser"):
    forAll: (config: Flamingos2Configs, newDisperser: Flamingos2Disperser) =>
      val modified = config.copy(disperser = newDisperser)
      config.disperser != newDisperser ==> {
        val matcher = Flamingos2LS
        !matcher.configsMatch(config, modified)
      }

  property("Flamingos2LS does not match configs with different filter"):
    forAll: (config: Flamingos2Configs, newFilter: Flamingos2Filter) =>
      val modified = config.copy(filter = newFilter)
      config.filter != newFilter ==> {
        val matcher = Flamingos2LS
        !matcher.configsMatch(config, modified)
      }

  property("Flamingos2LS does not match configs with different FPU"):
    forAll: (config: Flamingos2Configs, newFpu: Flamingos2Fpu) =>
      val modified = config.copy(fpu = newFpu)
      config.fpu != newFpu ==> {
        val matcher = Flamingos2LS
        !matcher.configsMatch(config, modified)
      }

  property("Flamingos2LS normalize is idempotent"):
    forAll: (config: Flamingos2Configs) =>
      val matcher = Flamingos2LS
      val normalized = matcher.normalize(config)
      val doubleNormalized = matcher.normalize(normalized)
      normalized === doubleNormalized
