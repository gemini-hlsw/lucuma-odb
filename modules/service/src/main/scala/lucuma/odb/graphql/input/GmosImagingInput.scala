// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.*
import lucuma.core.enums.GmosBinning
import lucuma.core.math.Offset
import lucuma.odb.data.Nullable
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

object GmosImagingInput:

  sealed trait Create[F] {
    def filters: NonEmptyList[F]
    def common:  Create.Common
  }

  object Create:

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

    case class North(
      filters: NonEmptyList[GmosNorthFilter],
      common:  Common
    ) extends Create[GmosNorthFilter]:

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthImaging

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (filters, offsets, exMfn, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            filters.flatMap(NonEmptyList.fromList).fold(
              Result.failure("At least one filter must be specified for GMOS imaging observations.")
            )(filters =>
              Result(
                North(filters,
                  Common(
                    exMfn.toOption,
                    exBin.toOption,
                    exAmpReadMode.toOption,
                    exAmpGain.toOption,
                    exRoi.toOption,
                    offsets.orEmpty
                  )
                )
              )
            )
        }

    case class South(
      filters: NonEmptyList[GmosSouthFilter],
      common:  Common
    ) extends Create[GmosSouthFilter]:

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthImaging

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap {
          case (filters, offsets, exMfn, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            filters.flatMap(NonEmptyList.fromList).fold(
              Result.failure("At least one filter must be specified for GMOS imaging observations.")
            )(filters =>
              Result(
                South(filters,
                  Common(
                    exMfn.toOption,
                    exBin.toOption,
                    exAmpReadMode.toOption,
                    exAmpGain.toOption,
                    exRoi.toOption,
                    offsets.orEmpty
                  )
                )
              )
            )
        }
  end Create

  object Edit:
    def validateFilters[A](a: Option[List[A]]): Result[Option[NonEmptyList[A]]] =
      a.fold(
        Result.success(none)
      )(fs =>
        NonEmptyList.fromList(fs).fold(
          Result.failure("At least one filter must be specified for GMOS imaging observations.")
          )(l => Result.success(l.some))
        )

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

    case class North(
      filters: Option[NonEmptyList[GmosNorthFilter]],
      common:  Common
    ):
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthImaging

      val toCreate: Result[Create.North] =
        filters.fold(
          Result.failure("At least one filter must be specified for GMOS imaging observations.")
        )(filterList => Result(Create.North(filterList, common.toCreate)))

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (filters, offsets, exMfm, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            validateFilters(filters).flatMap: filterList =>
              Result(
                North(
                  filterList,
                  Common(exMfm, exBin, exAmpReadMode, exAmpGain, exRoi, offsets.orEmpty)
                )
              )

    case class South(
      filters: Option[NonEmptyList[GmosSouthFilter]],
      common:  Common
    ):
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthImaging

      val toCreate: Result[Create.South] =
        filters.fold(
          Result.failure("At least one filter must be specified for GMOS imaging observations.")
        )(filterList => Result(Create.South(filterList, common.toCreate)))

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (filters, offsets, exMfm, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            validateFilters(filters).flatMap: filterList =>
              Result(South(
                filterList,
                Common(exMfm, exBin, exAmpReadMode, exAmpGain, exRoi, offsets.orEmpty)))
  end Edit

  private val NorthData: Matcher[(
    Option[List[GmosNorthFilter]],
    Option[List[Offset]],
    Nullable[MultipleFiltersMode],
    Nullable[GmosBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthFilterBinding.List.Option("filters", rFilters),
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
      ).parTupled

  private val SouthData: Matcher[(
    Option[List[GmosSouthFilter]],
    Option[List[Offset]],
    Nullable[MultipleFiltersMode],
    Nullable[GmosBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthFilterBinding.List.Option("filters", rFilters),
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
      ).parTupled

end GmosImagingInput
