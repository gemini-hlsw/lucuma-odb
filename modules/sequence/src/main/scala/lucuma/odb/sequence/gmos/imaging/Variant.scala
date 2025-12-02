// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.gmos.imaging

import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.math.Offset
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.sequence.data.OffsetGenerator
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism

sealed trait Variant:
  def variantType: VariantType =
    this match
      case Variant.Grouped(_, _, _, _)    => VariantType.Grouped
      case Variant.Interleaved            => VariantType.Interleaved
      case Variant.PreImaging(_, _, _, _) => VariantType.PreImaging

  def toGrouped: Variant.Grouped =
    this match
      case g @ Variant.Grouped(_, _, _, _) => g
      case _                               => Variant.Grouped.Default

  def toPreImaging: Variant.PreImaging =
    this match
      case p @ Variant.PreImaging(_, _, _, _) => p
      case _                                  => Variant.PreImaging.Default

object Variant:

  case class Grouped(
    order:      WavelengthOrder,
    offsets:    OffsetGenerator,
    skyCount:   NonNegInt,
    skyOffsets: OffsetGenerator
  ) extends Variant

  object Grouped:
    val Default: Grouped =
      Grouped(
        order      = WavelengthOrder.Decreasing,
        offsets    = OffsetGenerator.NoGenerator,
        skyCount   = NonNegInt.MinValue,
        skyOffsets = OffsetGenerator.NoGenerator
      )

    val offsets: Lens[Grouped, OffsetGenerator] =
      GenLens[Grouped](_.offsets)

    val skyOffsets: Lens[Grouped, OffsetGenerator] =
      GenLens[Grouped](_.skyOffsets)

  val grouped: Prism[Variant, Grouped] =
    GenPrism[Variant, Grouped]

  val offsets: Optional[Variant, OffsetGenerator] =
    grouped.andThen(Grouped.offsets)

  val skyOffsets: Optional[Variant, OffsetGenerator] =
    grouped.andThen(Grouped.skyOffsets)

  case object Interleaved extends Variant

  case class PreImaging(
    offset1: Offset,
    offset2: Offset,
    offset3: Offset,
    offset4: Offset
  ) extends Variant

  object PreImaging:
    val Default: PreImaging =
      PreImaging(
        offset1 = Offset.Zero,
        offset2 = Offset.Zero,
        offset3 = Offset.Zero,
        offset4 = Offset.Zero
      )