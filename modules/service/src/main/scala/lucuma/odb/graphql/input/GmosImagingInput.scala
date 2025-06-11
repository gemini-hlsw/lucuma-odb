// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.*
import lucuma.core.enums.GmosBinning
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import cats.data.NonEmptyList

object GmosImagingInput:

  sealed trait Create[F] {
    def filters: NonEmptyList[F]
    def common:  Create.Common
  }

  object Create:

    case class Common(
      explicitBin:         Option[GmosBinning],
      explicitAmpReadMode: Option[GmosAmpReadMode],
      explicitAmpGain:     Option[GmosAmpGain],
      explicitRoi:         Option[GmosRoi]
    )

    case class North(
      filters: NonEmptyList[GmosNorthFilter],
      common:  Common
    ) extends Create[GmosNorthFilter]:

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthImaging

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            filters.flatMap(NonEmptyList.fromList).fold(
              Result.failure("At least one filter must be specified for GMOS imaging observations.")
            )(filters =>
              Result(
                North(filters,
                  Common(
                    exBin.toOption,
                    exAmpReadMode.toOption,
                    exAmpGain.toOption,
                    exRoi.toOption
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
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            filters.flatMap(NonEmptyList.fromList).fold(
              Result.failure("At least one filter must be specified for GMOS imaging observations.")
            )(filters =>
              Result(
                South(filters,
                  Common(
                    exBin.toOption,
                    exAmpReadMode.toOption,
                    exAmpGain.toOption,
                    exRoi.toOption
                  )
                )
              )
            )
        }
  end Create

  object Edit:

    case class Common(
      explicitBin:         Nullable[GmosBinning],
      explicitAmpReadMode: Nullable[GmosAmpReadMode],
      explicitAmpGain:     Nullable[GmosAmpGain],
      explicitRoi:         Nullable[GmosRoi]
    ):

      def toCreate: Create.Common =
        Create.Common(
          explicitBin         = explicitBin.toOption,
          explicitAmpReadMode = explicitAmpReadMode.toOption,
          explicitAmpGain     = explicitAmpGain.toOption,
          explicitRoi         = explicitRoi.toOption
        )

    case class North(
      filters: Option[NonEmptyList[GmosNorthFilter]],
      common:  Common
    ):

      val toCreate: Result[Create.North] =
        filters.fold(
          Result.failure("At least one filter must be specified for GMOS imaging observations.")
        )(filterList => Result(Create.North(filterList, common.toCreate)))

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            Result(North(
              filters.flatMap(NonEmptyList.fromList),
              Common(exBin, exAmpReadMode, exAmpGain, exRoi)))

    case class South(
      filters: Option[NonEmptyList[GmosSouthFilter]],
      common:  Common
    ):

      val toCreate: Result[Create.South] =
        filters.fold(
          Result.failure("At least one filter must be specified for GMOS imaging observations.")
        )(filterList => Result(Create.South(filterList, common.toCreate)))

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            Result(South(
              filters.flatMap(NonEmptyList.fromList),
              Common(exBin, exAmpReadMode, exAmpGain, exRoi)))
  end Edit

  private val NorthData: Matcher[(
    Option[List[GmosNorthFilter]],
    Nullable[GmosBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthFilterBinding.List.Option("filters", rFilters),
        GmosBinningBinding.Nullable("explicitBin", rExplicitBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
      ) => (
        rFilters,
        rExplicitBin,
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi,
      ).parTupled

  private val SouthData: Matcher[(
    Option[List[GmosSouthFilter]],
    Nullable[GmosBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthFilterBinding.List.Option("filters", rFilters),
        GmosBinningBinding.Nullable("explicitBin", rExplicitBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
      ) => (
        rFilters,
        rExplicitBin,
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi,
      ).parTupled

end GmosImagingInput

