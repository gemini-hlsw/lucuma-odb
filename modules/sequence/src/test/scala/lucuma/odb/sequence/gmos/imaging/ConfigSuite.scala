// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import munit.FunSuite

import scala.collection.immutable.SortedMap

class ConfigSuite extends FunSuite {

  def bandNormalized[T]: BandNormalized[T] =
    BandNormalized(None, SortedMap.empty)

  private val sourceProfile = SourceProfile.Point(bandNormalized)
  private val imageQuality = ImageQuality.Preset.OnePointFive
  private val sampling = PosDouble.unsafeFrom(0.5)

  test("Config.GmosNorth computes binning based on source profile and image quality") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    assertEquals(config.bin, GmosBinning.Four) // Computed based on source profile and image quality
  }

  test("Config.GmosSouth computes binning based on source profile and image quality") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    assertEquals(config.bin, GmosBinning.Four) // Computed based on source profile and image quality
  }

  test("Config.GmosNorth explicit binning overrides default") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    assertEquals(config.bin, GmosBinning.One) // explicit value overrides default
  }

  test("Config.GmosSouth explicit binning overrides default") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    assertEquals(config.bin, GmosBinning.One) // explicit value overrides default
  }

  test("Config.GmosNorth has correct default values for all fields") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    assertEquals(config.bin, GmosBinning.Four) // Computed value
    assertEquals(config.ampReadMode, GmosAmpReadMode.Slow)
    assertEquals(config.ampGain, GmosAmpGain.Low)
    assertEquals(config.roi, GmosRoi.FullFrame)
  }

  test("Config.GmosSouth has correct default values for all fields") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    assertEquals(config.bin, GmosBinning.Four) // Computed value
    assertEquals(config.ampReadMode, GmosAmpReadMode.Slow)
    assertEquals(config.ampGain, GmosAmpGain.Low)
    assertEquals(config.roi, GmosRoi.FullFrame)
  }

  test("Config.GmosNorth ccdMode is correctly derived from binning") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = Some(GmosAmpReadMode.Fast),
      explicitAmpGain = Some(GmosAmpGain.High),
      explicitRoi = None
    )

    val ccdMode = config.ccdMode
    assertEquals(ccdMode.xBin, GmosXBinning.Two)
    assertEquals(ccdMode.yBin, GmosYBinning.Two)
    assertEquals(ccdMode.ampGain, GmosAmpGain.High)
    assertEquals(ccdMode.ampReadMode, GmosAmpReadMode.Fast)
  }

  test("Config.GmosSouth ccdMode is correctly derived from binning") {
    val filters = NonEmptyList.of(GmosSouthFilter.RPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitAmpReadMode = Some(GmosAmpReadMode.Slow),
      explicitAmpGain = Some(GmosAmpGain.Low),
      explicitRoi = None
    )

    val ccdMode = config.ccdMode
    assertEquals(ccdMode.xBin, GmosXBinning.One)
    assertEquals(ccdMode.yBin, GmosYBinning.One)
    assertEquals(ccdMode.ampGain, GmosAmpGain.Low)
    assertEquals(ccdMode.ampReadMode, GmosAmpReadMode.Slow)
  }

  test("Config.GmosNorth hashBytes produces consistent results") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime, GmosNorthFilter.RPrime)
    val config1 = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = Some(GmosAmpReadMode.Slow),
      explicitAmpGain = Some(GmosAmpGain.Low),
      explicitRoi = Some(GmosRoi.FullFrame)
    )
    val config2 = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = Some(GmosAmpReadMode.Slow),
      explicitAmpGain = Some(GmosAmpGain.Low),
      explicitRoi = Some(GmosRoi.FullFrame)
    )

    // Same configurations should produce same hash
    assertEquals(config1.hashBytes.toList, config2.hashBytes.toList)
  }

  test("Config.GmosSouth hashBytes produces different results for different configs") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config1 = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )
    val config2 = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    // Different configurations should produce different hashes
    assertNotEquals(config1.hashBytes.toList, config2.hashBytes.toList)
  }

  test("Config.GmosNorth.reconcile works with identical configs") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    val result = Config.GmosNorth.reconcile(config, List(config))
    assertEquals(result, Some(config))
  }

  test("Config.GmosNorth.reconcile fails with different configs") {
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config1 = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )
    val config2 = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    val result = Config.GmosNorth.reconcile(config1, List(config2))
    assertEquals(result, None)
  }

  test("Config.GmosSouth.reconcile works with identical configs") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    val result = Config.GmosSouth.reconcile(config, List(config))
    assertEquals(result, Some(config))
  }

  test("Config.GmosSouth.reconcile fails with different configs") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config1 = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )
    val config2 = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    val result = Config.GmosSouth.reconcile(config1, List(config2))
    assertEquals(result, None)
  }

}
