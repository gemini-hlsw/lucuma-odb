// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import cats.data.NonEmptyList
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.SpectralDefinition.BandNormalized
import munit.FunSuite

import scala.collection.immutable.SortedMap

class ConfigSuite extends FunSuite:

  def bandNormalized[T]: BandNormalized[T] =
    BandNormalized(None, SortedMap.empty)

  test("Config.GmosNorth explicit binning overrides default"):
    val config = Config.GmosNorth(
      variant = Variant.Interleaved.Default,
      NonEmptyList.of(GmosNorthFilter.GPrime),
      common  = Config.Common(
        defaultBin  = GmosBinning.Two,
        explicitBin = Some(GmosBinning.One)
      )
    )

    assertEquals(config.bin, GmosBinning.One) // explicit value overrides default

  test("Config.GmosSouth explicit binning overrides default"):
    val config = Config.GmosSouth(
      variant = Variant.Interleaved.Default,
      NonEmptyList.of(GmosSouthFilter.GPrime),
      common  = Config.Common(
        defaultBin  = GmosBinning.Two,
        explicitBin = Some(GmosBinning.One),
      )
    )

    assertEquals(config.bin, GmosBinning.One) // explicit value overrides default