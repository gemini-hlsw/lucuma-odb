// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.syntax.*
import lucuma.core.enums.GmosImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait GmosImagingVariantInput:
  def variantType: GmosImagingVariantType

object GmosImagingVariantInput:

  case class Grouped(
    order:      Option[WavelengthOrder],
    offsets:    Nullable[TelescopeConfigGeneratorInput],
    skyCount:   Option[NonNegInt],
    skyOffsets: Nullable[TelescopeConfigGeneratorInput]
  ) extends GmosImagingVariantInput:
    def variantType: GmosImagingVariantType =
      GmosImagingVariantType.Grouped

  val grouped: Prism[GmosImagingVariantInput, Grouped] =
    GenPrism[GmosImagingVariantInput, Grouped]

  case class Interleaved(
    offsets:    Nullable[TelescopeConfigGeneratorInput],
    skyCount:   Option[NonNegInt],
    skyOffsets: Nullable[TelescopeConfigGeneratorInput]
  ) extends GmosImagingVariantInput:
    def variantType: GmosImagingVariantType =
      GmosImagingVariantType.Interleaved

  val interleaved: Prism[GmosImagingVariantInput, Interleaved] =
    GenPrism[GmosImagingVariantInput, Interleaved]

  case class PreImaging(
    offset1: Option[Offset],
    offset2: Option[Offset],
    offset3: Option[Offset],
    offset4: Option[Offset]
  ) extends GmosImagingVariantInput:
    def variantType: GmosImagingVariantType =
      GmosImagingVariantType.PreImaging

  val preImaging: Prism[GmosImagingVariantInput, PreImaging] =
    GenPrism[GmosImagingVariantInput, PreImaging]

  val order: Optional[GmosImagingVariantInput, Option[WavelengthOrder]] =
    Optional[GmosImagingVariantInput, Option[WavelengthOrder]] {
      case Grouped(o, _, _, _) => o.some
      case _                   => none
    } { order => {
      case Grouped(_, offsets, skyCount, skyOffsets) => Grouped(order, offsets, skyCount, skyOffsets)
      case in                                        => in
    }}

  val offsets: Optional[GmosImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] =
    Optional[GmosImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] {
      case Grouped(_, o, _, _)  => o.some
      case Interleaved(o, _, _) => o.some
      case _                    => none
    } { gen => {
      case Grouped(order, _, skyCount, skyOffsets) => Grouped(order, gen, skyCount, skyOffsets)
      case Interleaved(_, skyCount, skyOffsets)    => Interleaved(gen, skyCount, skyOffsets)
      case in                                      => in
    }}

  val skyCount: Optional[GmosImagingVariantInput, Option[NonNegInt]] =
    Optional[GmosImagingVariantInput, Option[NonNegInt]] {
      case Grouped(_, _, c, _)  => c.some
      case Interleaved(_, c, _) => c.some
      case _                    => none
    } { skyCount => {
      case Grouped(order, offsets, _, skyOffsets) => Grouped(order, offsets, skyCount, skyOffsets)
      case Interleaved(offsets, _, skyOffsets)    => Interleaved(offsets, skyCount, skyOffsets)
      case in                                     => in
    }}

  val skyOffsets: Optional[GmosImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] =
    Optional[GmosImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] {
      case Grouped(_, _, _, o)  => o.some
      case Interleaved(_, _, o) => o.some
      case _                    => none
    } { gen => {
      case Grouped(order, offsets, skyCount, _) => Grouped(order, offsets, skyCount, gen)
      case Interleaved(offsets, skyCount, _)    => Interleaved(offsets, skyCount, gen)
      case in                                   => in
    }}

  val Default: GmosImagingVariantInput =
    Grouped(none, Nullable.Absent, none, Nullable.Absent)

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