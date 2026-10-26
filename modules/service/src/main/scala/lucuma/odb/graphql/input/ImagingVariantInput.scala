// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.ImagingVariantType
import lucuma.core.enums.WavelengthOrder
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import monocle.Optional
import monocle.Prism
import monocle.macros.GenPrism

sealed trait ImagingVariantInput:
  def variantType: ImagingVariantType

object ImagingVariantInput:

  case class Grouped(
    order:           Option[WavelengthOrder],
    offsets:         Nullable[TelescopeConfigGeneratorInput],
    skyCount:        Option[NonNegInt],
    skyOffsets:      Nullable[TelescopeConfigGeneratorInput],
    exposuresPerOffset: Option[PosInt]
  ) extends ImagingVariantInput:
    def variantType: ImagingVariantType =
      ImagingVariantType.Grouped

  val grouped: Prism[ImagingVariantInput, Grouped] =
    GenPrism[ImagingVariantInput, Grouped]

  case class Interleaved(
    offsets:            Nullable[TelescopeConfigGeneratorInput],
    skyCount:           Option[NonNegInt],
    skyOffsets:         Nullable[TelescopeConfigGeneratorInput],
    exposuresPerOffset: Option[PosInt]
  ) extends ImagingVariantInput:
    def variantType: ImagingVariantType =
      ImagingVariantType.Interleaved

  val interleaved: Prism[ImagingVariantInput, Interleaved] =
    GenPrism[ImagingVariantInput, Interleaved]

  case class PreImaging(
    offset1:            Option[Offset],
    offset2:            Option[Offset],
    offset3:            Option[Offset],
    offset4:            Option[Offset],
    exposuresPerOffset: Option[PosInt]
  ) extends ImagingVariantInput:
    def variantType: ImagingVariantType =
      ImagingVariantType.PreImaging

  val preImaging: Prism[ImagingVariantInput, PreImaging] =
    GenPrism[ImagingVariantInput, PreImaging]

  val order: Optional[ImagingVariantInput, Option[WavelengthOrder]] =
    Optional[ImagingVariantInput, Option[WavelengthOrder]] {
      case Grouped(o, _, _, _, _) => o.some
      case _                      => none
    } { order => {
      case g: Grouped => g.copy(order = order)
      case in         => in
    }}

  val offsets: Optional[ImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] =
    Optional[ImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] {
      case Grouped(_, o, _, _, _)  => o.some
      case Interleaved(o, _, _, _) => o.some
      case _                       => none
    } { gen => {
      case g: Grouped     => g.copy(offsets = gen)
      case i: Interleaved => i.copy(offsets = gen)
      case in             => in
    }}

  val skyCount: Optional[ImagingVariantInput, Option[NonNegInt]] =
    Optional[ImagingVariantInput, Option[NonNegInt]] {
      case Grouped(_, _, c, _, _)  => c.some
      case Interleaved(_, c, _, _) => c.some
      case _                       => none
    } { skyCount => {
      case g: Grouped     => g.copy(skyCount = skyCount)
      case i: Interleaved => i.copy(skyCount = skyCount)
      case in             => in
    }}

  val skyOffsets: Optional[ImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] =
    Optional[ImagingVariantInput, Nullable[TelescopeConfigGeneratorInput]] {
      case Grouped(_, _, _, o, _)  => o.some
      case Interleaved(_, _, o, _) => o.some
      case _                       => none
    } { gen => {
      case g: Grouped     => g.copy(skyOffsets = gen)
      case i: Interleaved => i.copy(skyOffsets = gen)
      case in             => in
    }}

  val exposuresPerOffset: Optional[ImagingVariantInput, Option[PosInt]] =
    Optional[ImagingVariantInput, Option[PosInt]] {
      case Grouped(_, _, _, _, k)    => k.some
      case Interleaved(_, _, _, k)   => k.some
      case PreImaging(_, _, _, _, k) => k.some
    } { k => {
      case g: Grouped     => g.copy(exposuresPerOffset = k)
      case i: Interleaved => i.copy(exposuresPerOffset = k)
      case p: PreImaging  => p.copy(exposuresPerOffset = k)
    }}

  val Default: ImagingVariantInput =
    Grouped(none, Nullable.Absent, none, Nullable.Absent, none)

  val Binding: Matcher[ImagingVariantInput] =
    val GroupedBinding: Matcher[ImagingVariantInput] =
      ObjectFieldsBinding.rmap:
        case List(
          WavelengthOrderBinding.Option("order", rOrder),
          TelescopeConfigGeneratorInput.Binding.Nullable("offsets", rOffsets),
          NonNegIntBinding.Option("skyCount", rSkyCount),
          TelescopeConfigGeneratorInput.Binding.Nullable("skyOffsets", rSkyOffsets),
          PosIntBinding.Option("exposuresPerOffset", rExposuresPerOffset)
        ) => (rOrder, rOffsets, rSkyCount, rSkyOffsets, rExposuresPerOffset).parMapN:
          (order, offsets, skyCount, skyOffsets, exposuresPerOffset) =>
            Grouped(order, offsets, skyCount, skyOffsets, exposuresPerOffset)

    val InterleavedBinding: Matcher[ImagingVariantInput] =
      ObjectFieldsBinding.rmap:
        case List(
          TelescopeConfigGeneratorInput.Binding.Nullable("offsets", rOffsets),
          NonNegIntBinding.Option("skyCount", rSkyCount),
          TelescopeConfigGeneratorInput.Binding.Nullable("skyOffsets", rSkyOffsets),
          PosIntBinding.Option("exposuresPerOffset", rExposuresPerOffset)
        ) => (rOffsets, rSkyCount, rSkyOffsets, rExposuresPerOffset).parMapN:
          (offsets, skyCount, skyOffsets, exposuresPerOffset) =>
            Interleaved(offsets, skyCount, skyOffsets, exposuresPerOffset)

    val PreImagingBinding: Matcher[ImagingVariantInput] =
      ObjectFieldsBinding.rmap:
        case List(
          OffsetInput.Binding.Option("offset1", rOffset1),
          OffsetInput.Binding.Option("offset2", rOffset2),
          OffsetInput.Binding.Option("offset3", rOffset3),
          OffsetInput.Binding.Option("offset4", rOffset4),
          PosIntBinding.Option("exposuresPerOffset", rExposuresPerOffset)
        ) => (rOffset1, rOffset2, rOffset3, rOffset4, rExposuresPerOffset).parMapN:
          (o1, o2, o3, o4, exposuresPerOffset) =>
            PreImaging(o1, o2, o3, o4, exposuresPerOffset)

    OneOfBinding(
      "grouped"     -> GroupedBinding,
      "interleaved" -> InterleavedBinding,
      "preImaging"  -> PreImagingBinding
    )