// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.syntax.*
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.core.enums.WavelengthOrder
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.TelescopeConfigGenerator
import lucuma.odb.sequence.gmos.imaging.Variant
import lucuma.odb.sequence.gmos.imaging.VariantType

sealed trait GmosImagingVariantInput:
  def toVariant: Variant
  def variantType: VariantType

object GmosImagingVariantInput:

  case class Grouped(
    order:      Option[WavelengthOrder],
    offsets:    Nullable[TelescopeConfigGenerator],
    skyCount:   Option[NonNegInt],
    skyOffsets: Nullable[TelescopeConfigGenerator]
  ) extends GmosImagingVariantInput:
    def toVariant: Variant =
      Variant.Grouped(
        order.getOrElse(WavelengthOrder.Increasing),
        offsets.toOption.getOrElse(TelescopeConfigGenerator.NoGenerator),
        skyCount.getOrElse(NonNegInt.unsafeFrom(0)),
        skyOffsets.toOption.getOrElse(TelescopeConfigGenerator.NoGenerator)
      )
    def variantType: VariantType =
      VariantType.Grouped

  case class Interleaved(
    offsets:    Nullable[TelescopeConfigGenerator],
    skyCount:   Option[NonNegInt],
    skyOffsets: Nullable[TelescopeConfigGenerator]
  ) extends GmosImagingVariantInput:
    def toVariant: Variant =
      Variant.Interleaved(
        offsets.toOption.getOrElse(TelescopeConfigGenerator.NoGenerator),
        skyCount.getOrElse(NonNegInt.unsafeFrom(0)),
        skyOffsets.toOption.getOrElse(TelescopeConfigGenerator.NoGenerator)
      )
    def variantType: VariantType =
      VariantType.Interleaved

  case class PreImaging(
    offset1: Option[Offset],
    offset2: Option[Offset],
    offset3: Option[Offset],
    offset4: Option[Offset]
  ) extends GmosImagingVariantInput:
    def toVariant: Variant =
      Variant.PreImaging(
        offset1.getOrElse(Offset.Zero),
        offset2.getOrElse(Offset.Zero),
        offset3.getOrElse(Offset.Zero),
        offset4.getOrElse(Offset.Zero)
      )
    def variantType: VariantType =
      VariantType.PreImaging

  val Binding: Matcher[GmosImagingVariantInput] =
    val GroupedBinding: Matcher[GmosImagingVariantInput] =
      ObjectFieldsBinding.rmap:
        case List(
          WavelengthOrderBinding.Option("order", rOrder),
          TelescopeConfigGeneratorInput.Binding.Nullable("offsets", rOffsets),
          NonNegIntBinding.Option("skyCount", rSkyCount),
          TelescopeConfigGeneratorInput.Binding.Nullable("skyOffsets", rSkyOffsets)
        ) => (rOrder, rOffsets, rSkyCount, rSkyOffsets).parMapN:
          (order, offsets, skyCount, skyOffsets) =>
            Grouped(order, offsets, skyCount, skyOffsets)

    val InterleavedBinding: Matcher[GmosImagingVariantInput] =
      ObjectFieldsBinding.rmap:
        case List(
          TelescopeConfigGeneratorInput.Binding.Nullable("offsets", rOffsets),
          NonNegIntBinding.Option("skyCount", rSkyCount),
          TelescopeConfigGeneratorInput.Binding.Nullable("skyOffsets", rSkyOffsets)
        ) => (rOffsets, rSkyCount, rSkyOffsets).parMapN:
          (offsets, skyCount, skyOffsets) =>
            Interleaved(offsets, skyCount, skyOffsets)

    val PreImagingBinding: Matcher[GmosImagingVariantInput] =
      ObjectFieldsBinding.rmap:
        case List(
          OffsetInput.Binding.Option("offset1", rOffset1),
          OffsetInput.Binding.Option("offset2", rOffset2),
          OffsetInput.Binding.Option("offset3", rOffset3),
          OffsetInput.Binding.Option("offset4", rOffset4)
        ) => (rOffset1, rOffset2, rOffset3, rOffset4).parMapN: (o1, o2, o3, o4) =>
          PreImaging(o1, o2, o3, o4)

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