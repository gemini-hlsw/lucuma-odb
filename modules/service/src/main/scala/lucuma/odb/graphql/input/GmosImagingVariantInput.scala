// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.syntax.*
import lucuma.core.math.Offset
import lucuma.odb.data.GmosImagingVariantType
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.graphql.binding.*
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.macros.GenLens
import monocle.macros.GenPrism


sealed trait GmosImagingVariantInput:

  import GmosImagingVariantInput.*

  def variantType: GmosImagingVariantType =
    this match
      case Grouped(_, _, _, _)    => GmosImagingVariantType.Grouped
      case Interleaved            => GmosImagingVariantType.Interleaved
      case PreImaging(_, _, _, _) => GmosImagingVariantType.PreImaging

  def toGrouped: Grouped =
    this match
      case g @ Grouped(_, _, _, _) => g
      case _                       => Grouped.Default

  def toPreImaging: PreImaging =
    this match
      case p @ PreImaging(_, _, _, _) => p
      case _                          => PreImaging.Default

object GmosImagingVariantInput:

  case class Grouped(
    order:      WavelengthOrder,
    offsets:    OffsetGeneratorInput,
    skyCount:   NonNegInt,
    skyOffsets: OffsetGeneratorInput
  ) extends GmosImagingVariantInput

  object Grouped:
    val Default: Grouped =
      Grouped(
        order      = WavelengthOrder.Decreasing,
        offsets    = OffsetGeneratorInput.NoGenerator,
        skyCount   = NonNegInt.MinValue,
        skyOffsets = OffsetGeneratorInput.NoGenerator
      )

    val offsets: Lens[Grouped, OffsetGeneratorInput] =
      GenLens[Grouped](_.offsets)

    val skyOffsets: Lens[Grouped, OffsetGeneratorInput] =
      GenLens[Grouped](_.skyOffsets)

  val grouped: Prism[GmosImagingVariantInput, Grouped] =
    GenPrism[GmosImagingVariantInput, Grouped]

  val offsets: Optional[GmosImagingVariantInput, OffsetGeneratorInput] =
    grouped.andThen(Grouped.offsets)

  val skyOffsets: Optional[GmosImagingVariantInput, OffsetGeneratorInput] =
    grouped.andThen(Grouped.skyOffsets)

  case object Interleaved extends GmosImagingVariantInput

  case class PreImaging(
    offset1: Offset,
    offset2: Offset,
    offset3: Offset,
    offset4: Offset
  ) extends GmosImagingVariantInput

  object PreImaging:
    val Default: PreImaging =
      PreImaging(
        offset1 = Offset.Zero,
        offset2 = Offset.Zero,
        offset3 = Offset.Zero,
        offset4 = Offset.Zero
      )

  private val GroupedBinding: Matcher[Grouped] =
    ObjectFieldsBinding.rmap:
      case List(
        WavelengthOrderBinding.Option("order", rOrder),
        OffsetGeneratorInput.Binding.Option("offsets", rOffsets),
        NonNegIntBinding.Option("skyCount", rSkyCount),
        OffsetGeneratorInput.Binding.Option("skyOffsets", rSkyOffsets)
      ) => (rOrder, rOffsets, rSkyCount, rSkyOffsets).parMapN: (order, offsets, skyCount, skyOffsets) =>
        Grouped(
          order.getOrElse(WavelengthOrder.Decreasing),
          offsets.getOrElse(OffsetGeneratorInput.NoGenerator),
          skyCount.getOrElse(NonNegInt.unsafeFrom(0)),
          skyOffsets.getOrElse(OffsetGeneratorInput.NoGenerator)
        )

  private val InterleavedBinding: Matcher[Interleaved.type] =
    ObjectFieldsBinding.rmap:
      case List(
        BooleanBinding.Option("_placeholder", rPlace)
      ) => rPlace.as(Interleaved)

  private val PreImagingBinding: Matcher[PreImaging] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetInput.Binding("offset1", rOffset1),
        OffsetInput.Binding("offset2", rOffset2),
        OffsetInput.Binding("offset3", rOffset3),
        OffsetInput.Binding("offset4", rOffset4)
      ) => (rOffset1, rOffset2, rOffset3, rOffset4).parMapN: (o1, o2, o3, o4) =>
        PreImaging(o1, o2, o3, o4)

  val Binding: Matcher[GmosImagingVariantInput] =
    ObjectFieldsBinding.rmap:
      case List(
        GroupedBinding.Option("grouped", rGrouped),
        InterleavedBinding.Option("interleaved", rInterleaved),
        PreImagingBinding.Option("preImaging", rPreImaging)
      ) => (rGrouped, rInterleaved, rPreImaging).parTupled.flatMap:
        case (Some(g), None, None) => g.success
        case (None, Some(i), None) => i.success
        case (None, None, Some(p)) => p.success
        case _                     => Matcher.validationFailure(s"Exactly one of 'grouped', 'interleaved' or 'preImaging' is required.")