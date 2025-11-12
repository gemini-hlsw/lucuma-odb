// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.*
import lucuma.core.enums.GmosBinning
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object GmosImagingInput:

  // Create ---------------------------------------------------------------------

  case class Create[F](
    filters: NonEmptyList[GmosImagingFilterInput[F]],
    common:  Create.Common
  )

  object Create:

    type North = Create[GmosNorthFilter]
    type South = Create[GmosSouthFilter]

    case class Common(
      explicitMultipleFiltersMode: Option[MultipleFiltersMode],
      explicitBin:                 Option[GmosBinning],
      explicitAmpReadMode:         Option[GmosAmpReadMode],
      explicitAmpGain:             Option[GmosAmpGain],
      explicitRoi:                 Option[GmosRoi],
      offsets:                     List[Offset]
    ):
      // Formatted to store in a text column in the database
      val formattedOffsets: String =
        if (offsets.isEmpty) "" else OffsetsFormat.reverseGet(offsets)

    private def binding[F](
      FilterBinding: Matcher[GmosImagingFilterInput[F]]
    ): Matcher[Create[F]] =
      ObjectFieldsBinding.rmap:
        case List(
          FilterBinding.List("filters", rFilters),
          OffsetInput.Binding.List.Option("offsets", rOffsets),
          MultipleFiltersModeBinding.Option("explicitMultipleFiltersMode", rExplicitMultipleFiltersMode),
          GmosBinningBinding.Option("explicitBin", rExplicitBin),
          GmosAmpReadModeBinding.Option("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Option("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Option("explicitRoi", rExplicitRoi),
        ) => (
          rFilters,
          rOffsets,
          rExplicitMultipleFiltersMode,
          rExplicitBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
        ).parTupled.flatMap:
          case (filters, offsets, exMfn, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            NonEmptyList.fromList(filters).fold(
              OdbError.InvalidArgument("At least one filter must be specified for GMOS imaging observations.".some).asFailure
            ): filters =>
              Result(
                Create(
                  filters,
                  Common(exMfn, exBin, exAmpReadMode, exAmpGain, exRoi, offsets.orEmpty)
                )
              )

    val NorthBinding: Matcher[Create[GmosNorthFilter]] =
      binding(GmosImagingFilterInput.NorthBinding)


    val SouthBinding: Matcher[Create[GmosSouthFilter]] =
      binding(GmosImagingFilterInput.SouthBinding)

  end Create

  // Edit ---------------------------------------------------------------------

  case class Edit[F](
    filters: Option[NonEmptyList[GmosImagingFilterInput[F]]],
    common:  Edit.Common
  ):
    def toCreate: Result[Create[F]] =
      filters.fold(
        OdbError.InvalidArgument("At least one filter must be specified for GMOS imaging observations.".some).asFailure
      )(fs => Create(fs, common.toCreate).success)

  object Edit:

    type North = Edit[GmosNorthFilter]
    type South = Edit[GmosSouthFilter]

    case class Common(
      explicitMultipleFiltersMode: Nullable[MultipleFiltersMode],
      explicitBin:                 Nullable[GmosBinning],
      explicitAmpReadMode:         Nullable[GmosAmpReadMode],
      explicitAmpGain:             Nullable[GmosAmpGain],
      explicitRoi:                 Nullable[GmosRoi],
      offsets:                     List[Offset]
    ):
      // Formatted to store in a text column in the database
      val formattedOffsets: String =
        if (offsets.isEmpty) "" else OffsetsFormat.reverseGet(offsets)

      def toCreate: Create.Common =
        Create.Common(
          explicitMultipleFiltersMode = explicitMultipleFiltersMode.toOption,
          explicitBin                 = explicitBin.toOption,
          explicitAmpReadMode         = explicitAmpReadMode.toOption,
          explicitAmpGain             = explicitAmpGain.toOption,
          explicitRoi                 = explicitRoi.toOption,
          offsets                     = offsets
        )

    private def binding[F](
      FilterBinding: Matcher[GmosImagingFilterInput[F]]
    ): Matcher[Edit[F]] =
      ObjectFieldsBinding.rmap:
        case List(
          FilterBinding.List.Option("filters", rFilters),
          OffsetInput.Binding.List.Option("offsets", rOffsets),
          MultipleFiltersModeBinding.Nullable("explicitMultipleFiltersMode", rExplicitMultipleFiltersMode),
          GmosBinningBinding.Nullable("explicitBin", rExplicitBin),
          GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        ) => (
          rFilters,
          rOffsets,
          rExplicitMultipleFiltersMode,
          rExplicitBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
        ).parTupled.flatMap:
          case (filters, offsets, exMfm, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            filters
              .traverse: fs =>
                NonEmptyList.fromList(fs).fold(
                  OdbError.InvalidArgument("At least one filter must be specified for GMOS imaging observations.".some).asFailure
                )(_.success)
              .map: fsUpdate =>
                Edit(
                  fsUpdate,
                  Common(exMfm, exBin, exAmpReadMode, exAmpGain, exRoi, offsets.orEmpty)
                )

    val NorthBinding: Matcher[Edit[GmosNorthFilter]] =
      binding(GmosImagingFilterInput.NorthBinding)

    val SouthBinding: Matcher[Edit[GmosSouthFilter]] =
      binding(GmosImagingFilterInput.SouthBinding)

  end Edit

end GmosImagingInput