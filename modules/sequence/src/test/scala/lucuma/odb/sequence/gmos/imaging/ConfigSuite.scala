// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosDouble
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ImageQuality
import lucuma.core.model.SourceProfile
import lucuma.core.model.SpectralDefinition.BandNormalized
import munit.FunSuite

import scala.collection.immutable.SortedMap

class ConfigSuite extends FunSuite:

  def bandNormalized[T]: BandNormalized[T] =
    BandNormalized(None, SortedMap.empty)

  private val sourceProfile = SourceProfile.Point(bandNormalized)
  private val imageQuality = ImageQuality.Preset.OnePointFive
  private val sampling = PosDouble.unsafeFrom(0.5)

  test("Config.GmosNorth computes binning based on source profile and image quality"):
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters
    )

    assertEquals(config.bin, GmosBinning.Four) // Computed based on source profile and image quality

  test("Config.GmosSouth computes binning based on source profile and image quality") {
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters
    )

    assertEquals(config.bin, GmosBinning.Four) // Computed based on source profile and image quality
  }

  test("Config.GmosNorth explicit binning overrides default"):
    val filters = NonEmptyList.of(GmosNorthFilter.GPrime)
    val config = Config.GmosNorth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One)
    )

    assertEquals(config.bin, GmosBinning.One) // explicit value overrides default

  test("Config.GmosSouth explicit binning overrides default"):
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
    )

    assertEquals(config.bin, GmosBinning.One) // explicit value overrides default

  test("Config.GmosSouth.reconcile works with identical configs"):
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.Two),
      explicitMultipleFiltersMode = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    val result = Config.GmosSouth.reconcile(config, List(config))
    assertEquals(result, Some(config))

  test("Config.GmosSouth.reconcile fails with different configs"):
    val filters = NonEmptyList.of(GmosSouthFilter.GPrime)
    val config1 = Config.GmosSouth(
      sourceProfile = sourceProfile,
      imageQuality = imageQuality,
      sampling = sampling,
      filters = filters,
      explicitBin = Some(GmosBinning.One),
      explicitMultipleFiltersMode = None,
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
      explicitMultipleFiltersMode = None,
      explicitAmpReadMode = None,
      explicitAmpGain = None,
      explicitRoi = None
    )

    val result = Config.GmosSouth.reconcile(config1, List(config2))
    assertEquals(result, None)
