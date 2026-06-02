// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.*
import lucuma.core.enums.GmosBinning
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*

object GmosImagingInput extends ImagingFilterCheck:

  // Create ---------------------------------------------------------------------

  case class Create[L](
    variant: GmosImagingVariantInput,
    filters: NonEmptyList[L],
    common:  Create.Common
  )

  object Create:

    type North = Create[GmosImagingFilterInput[GmosNorthFilter]]
    type South = Create[GmosImagingFilterInput[GmosSouthFilter]]

    case class Common(
      explicitBin:         Option[GmosBinning],
      explicitAmpReadMode: Option[GmosAmpReadMode],
      explicitAmpGain:     Option[GmosAmpGain],
      explicitRoi:         Option[GmosRoi]
    )

    private def binding[L](
      FilterBinding: Matcher[L]
    ): Matcher[Create[L]] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosImagingVariantInput.Binding("variant", rVariant),
          FilterBinding.List("filters", rFilters),
          GmosBinningBinding.Option("explicitBin", rExplicitBin),
          GmosAmpReadModeBinding.Option("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Option("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Option("explicitRoi", rExplicitRoi),
        ) => (
          rVariant,
          notEmpty("GMOS", rFilters),
          rExplicitBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
        ).parMapN: (variant, filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
          Create(variant, filters, Common(exBin, exAmpReadMode, exAmpGain, exRoi))

    val NorthBinding: Matcher[North] =
      binding(GmosImagingFilterInput.NorthBinding)


    val SouthBinding: Matcher[South] =
      binding(GmosImagingFilterInput.SouthBinding)

  end Create

  // Edit ---------------------------------------------------------------------

  case class Edit[L](
    variant: Option[GmosImagingVariantInput],
    filters: Option[NonEmptyList[L]],
    common:  Edit.Common
  ):
    def toCreate: Result[Create[L]] =
      for
        v  <- Result.fromOption(variant, OdbError.InvalidArgument("An imaging variant must be suplied for GMOS imaging observations".some).asProblem)
        fs <- Result.fromOption(filters, atLeastOne("GMOS").asProblem)
      yield Create(v, fs, common.toCreate)

  object Edit:

    type North = Edit[GmosImagingFilterInput[GmosNorthFilter]]
    type South = Edit[GmosImagingFilterInput[GmosSouthFilter]]

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

    private def binding[L](
      FilterBinding: Matcher[GmosImagingFilterInput[L]]
    ): Matcher[Edit[GmosImagingFilterInput[L]]] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosImagingVariantInput.Binding.Option("variant", rVariant),
          FilterBinding.List.Option("filters", rFilters),
          GmosBinningBinding.Nullable("explicitBin", rExplicitBin),
          GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi)
        ) => (
          rVariant,
          notEmptyIfPresent("GMOS", rFilters),
          rExplicitBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
        ).parMapN: (variant, filters, exBin, exAmpReadMode, exAmpGain, exRoi) =>
          Edit(variant, filters, Common(exBin, exAmpReadMode, exAmpGain, exRoi))

    val NorthBinding: Matcher[North] =
      binding(GmosImagingFilterInput.NorthBinding)

    val SouthBinding: Matcher[South] =
      binding(GmosImagingFilterInput.SouthBinding)

  end Edit

end GmosImagingInput
