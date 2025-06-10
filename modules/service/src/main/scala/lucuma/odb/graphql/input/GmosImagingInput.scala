// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.*
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*
import lucuma.core.enums.GmosBinning

object GmosImagingInput:

  sealed trait Create[F] {
    def filters: List[F]
    def common:  Create.Common
  }

  object Create:

    case class Common(
      explicitBin:      Option[GmosBinning],
      explicitAmpReadMode: Option[GmosAmpReadMode],
      explicitAmpGain:   Option[GmosAmpGain],
      explicitRoi:       Option[GmosRoi]
    )

    object Common:

      val Empty: Common =
        Common(
          explicitBin      = none,
          explicitAmpReadMode = none,
          explicitAmpGain   = none,
          explicitRoi       = none
        )


    case class North(
      filters: List[GmosNorthFilter],
      common:  Common
    ) extends Create[GmosNorthFilter]:

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthImaging

    object North:

      val Empty: North =
        North(
          filters = List.empty,
          common  = Common.Empty
        )

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            Result(
              North(filters.toOption.orEmpty,
                Common(
                  exBin.toOption,
                  exAmpReadMode.toOption,
                  exAmpGain.toOption,
                  exRoi.toOption
                )
              )
            )
        }

    case class South(
      filters: List[GmosSouthFilter],
      common:  Common
    ) extends Create[GmosSouthFilter]:

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthImaging

    object South:

      val Empty: South =
        South(
          filters = List.empty,
          common  = Common.Empty
        )

      val Binding: Matcher[South] =
        SouthData.rmap {
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            Result(
              South(filters.toOption.orEmpty,
                Common(
                  exBin.toOption,
                  exAmpReadMode.toOption,
                  exAmpGain.toOption,
                  exRoi.toOption
                )
              )
            )
        }
  end Create

  object Edit:

    case class Common(
      explicitBin:      Nullable[GmosBinning],
      explicitAmpReadMode: Nullable[GmosAmpReadMode],
      explicitAmpGain:   Nullable[GmosAmpGain],
      explicitRoi:       Nullable[GmosRoi]
    ):

      def toCreate: Create.Common =
        Create.Common(
          explicitBin      = explicitBin.toOption,
          explicitAmpReadMode = explicitAmpReadMode.toOption,
          explicitAmpGain   = explicitAmpGain.toOption,
          explicitRoi       = explicitRoi.toOption
        )

    object Common:

      val Empty: Common =
        Common(
          explicitBin      = Nullable.Absent,
          explicitAmpReadMode = Nullable.Absent,
          explicitAmpGain   = Nullable.Absent,
          explicitRoi       = Nullable.Absent
        )

    case class North(
      filters: Option[List[GmosNorthFilter]],
      common:  Common
    ):

      val toCreate: Result[Create.North] =
        Result(Create.North(filters.orEmpty, common.toCreate))

    object North:

      val Empty: North =
        North(
          filters = none,
          common  = Common.Empty
        )

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            Result(North(
              filters.toOption,
              Common(exBin, exAmpReadMode, exAmpGain, exRoi)))

    case class South(
      filters: Option[List[GmosSouthFilter]],
      common:  Common
    ):

      val toCreate: Result[Create.South] =
        Result(Create.South(filters.orEmpty, common.toCreate))

    object South:

      val Empty: South =
        South(
          filters = none,
          common  = Common.Empty
        )

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
            Result(South(
              filters.toOption,
              Common(exBin, exAmpReadMode, exAmpGain, exRoi)))
  end Edit

  private val NorthData: Matcher[(
    Nullable[List[GmosNorthFilter]],
    Nullable[GmosBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthFilterBinding.List.Nullable("filter", rFilters),
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
    Nullable[List[GmosSouthFilter]],
    Nullable[GmosBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthFilterBinding.List.Nullable("filter", rFilters),
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

