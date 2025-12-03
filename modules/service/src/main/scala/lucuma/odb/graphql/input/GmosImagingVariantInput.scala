// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.data.NonEmptyList
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.Result
import grackle.syntax.*
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.OffsetGenerator
import lucuma.odb.sequence.gmos.imaging.Variant

sealed trait GmosImagingVariantInput[L]:
  def create: Result[Variant[L]]

  def filters: Option[NonEmptyList[L]]

object GmosImagingVariantInput:

  private object FilterCheck:
    val AtLeastOne: OdbError =
      OdbError.InvalidArgument("At least one filter must be specified for GMOS imaging observations.".some)

    def notEmpty[L](fs: Option[NonEmptyList[L]]): Result[NonEmptyList[L]] =
      Result.fromOption(fs, AtLeastOne.asProblem)

    def notEmptyIfPresent[L](fs: Result[Option[List[L]]]): Result[Option[NonEmptyList[L]]] =
      fs.flatMap(_.traverse(fs => Result.fromOption(NonEmptyList.fromList(fs), AtLeastOne.asProblem)))

  case class Grouped[L](
    filters:    Option[NonEmptyList[L]],
    order:      Option[WavelengthOrder],
    offsets:    Nullable[OffsetGenerator],
    skyCount:   Option[NonNegInt],
    skyOffsets: Nullable[OffsetGenerator]
  ) extends GmosImagingVariantInput[L]:
    def create: Result[Variant[L]] =
      FilterCheck.notEmpty(filters).map: fs =>
        Variant.Grouped(
          fs,
          order.getOrElse(WavelengthOrder.Decreasing),
          offsets.toOption.getOrElse(OffsetGenerator.NoGenerator),
          skyCount.getOrElse(NonNegInt.unsafeFrom(0)),
          skyOffsets.toOption.getOrElse(OffsetGenerator.NoGenerator)
        )

  case class Interleaved[L](
    filters: Option[NonEmptyList[L]]
  ) extends GmosImagingVariantInput[L]:
    def create: Result[Variant[L]] =
      FilterCheck.notEmpty(filters).map(Variant.Interleaved.apply)

  case class PreImaging[L](
    filters: Option[NonEmptyList[L]],
    offset1: Option[Offset],
    offset2: Option[Offset],
    offset3: Option[Offset],
    offset4: Option[Offset]
  ) extends GmosImagingVariantInput[L]:
    def create: Result[Variant[L]] =
      FilterCheck.notEmpty(filters).map: fs =>
        Variant.PreImaging(
          fs,
          offset1.getOrElse(Offset.Zero),
          offset2.getOrElse(Offset.Zero),
          offset3.getOrElse(Offset.Zero),
          offset4.getOrElse(Offset.Zero)
        )

  def binding[L](
    FilterBinding: Matcher[L]
  ): Matcher[GmosImagingVariantInput[L]] =

    val GroupedBinding: Matcher[GmosImagingVariantInput[L]] =
      ObjectFieldsBinding.rmap:
        case List(
          FilterBinding.List.Option("filters", rFilters),
          WavelengthOrderBinding.Option("order", rOrder),
          OffsetGeneratorInput.Binding.Nullable("offsets", rOffsets),
          NonNegIntBinding.Option("skyCount", rSkyCount),
          OffsetGeneratorInput.Binding.Nullable("skyOffsets", rSkyOffsets)
        ) => (FilterCheck.notEmptyIfPresent(rFilters), rOrder, rOffsets, rSkyCount, rSkyOffsets).parMapN:
          (filters, order, offsets, skyCount, skyOffsets) =>
            Grouped(filters, order, offsets, skyCount, skyOffsets)

    val InterleavedBinding: Matcher[GmosImagingVariantInput[L]] =
      ObjectFieldsBinding.rmap:
        case List(
          FilterBinding.List.Option("filters", rFilters)
        ) =>
          FilterCheck.notEmptyIfPresent(rFilters).map(Interleaved.apply)

    val PreImagingBinding: Matcher[GmosImagingVariantInput[L]] =
      ObjectFieldsBinding.rmap:
        case List(
          FilterBinding.List.Option("filters", rFilters),
          OffsetInput.Binding.Option("offset1", rOffset1),
          OffsetInput.Binding.Option("offset2", rOffset2),
          OffsetInput.Binding.Option("offset3", rOffset3),
          OffsetInput.Binding.Option("offset4", rOffset4)
        ) => (FilterCheck.notEmptyIfPresent(rFilters), rOffset1, rOffset2, rOffset3, rOffset4).parMapN: (filters, o1, o2, o3, o4) =>
          PreImaging(filters, o1, o2, o3, o4)

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