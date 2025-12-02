// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.functor.*
import cats.syntax.parallel.*
import eu.timepit.refined.types.numeric.NonNegInt
import grackle.syntax.*
import lucuma.odb.data.WavelengthOrder
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.data.OffsetGenerator
import lucuma.odb.sequence.gmos.imaging.Variant

object GmosImagingVariantInput:

  private val GroupedBinding: Matcher[Variant.Grouped] =
    ObjectFieldsBinding.rmap:
      case List(
        WavelengthOrderBinding.Option("order", rOrder),
        OffsetGeneratorInput.Binding.Option("offsets", rOffsets),
        NonNegIntBinding.Option("skyCount", rSkyCount),
        OffsetGeneratorInput.Binding.Option("skyOffsets", rSkyOffsets)
      ) => (rOrder, rOffsets, rSkyCount, rSkyOffsets).parMapN: (order, offsets, skyCount, skyOffsets) =>
        Variant.Grouped(
          order.getOrElse(WavelengthOrder.Decreasing),
          offsets.getOrElse(OffsetGenerator.NoGenerator),
          skyCount.getOrElse(NonNegInt.unsafeFrom(0)),
          skyOffsets.getOrElse(OffsetGenerator.NoGenerator)
        )

  private val InterleavedBinding: Matcher[Variant.Interleaved.type] =
    ObjectFieldsBinding.rmap:
      case List(
        BooleanBinding.Option("_placeholder", rPlace)
      ) => rPlace.as(Variant.Interleaved)

  private val PreImagingBinding: Matcher[Variant.PreImaging] =
    ObjectFieldsBinding.rmap:
      case List(
        OffsetInput.Binding("offset1", rOffset1),
        OffsetInput.Binding("offset2", rOffset2),
        OffsetInput.Binding("offset3", rOffset3),
        OffsetInput.Binding("offset4", rOffset4)
      ) => (rOffset1, rOffset2, rOffset3, rOffset4).parMapN: (o1, o2, o3, o4) =>
        Variant.PreImaging(o1, o2, o3, o4)

  val Binding: Matcher[Variant] =
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