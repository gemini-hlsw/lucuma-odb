// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.enums.*
import lucuma.core.enums.GmosBinning
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.sequence.gmos.imaging.Variant

object GmosImagingInput:

  // Create ---------------------------------------------------------------------

  case class Create[L](
    variant: Variant[L],
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
      val VariantBinding = GmosImagingVariantInput.binding(FilterBinding)
      ObjectFieldsBinding.rmap:
        case List(
          VariantBinding("variant", rVariant),
          GmosBinningBinding.Option("explicitBin", rExplicitBin),
          GmosAmpReadModeBinding.Option("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Option("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Option("explicitRoi", rExplicitRoi),
        ) => (
          rVariant,
          rExplicitBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
        ).parTupled.flatMap: (variant, exBin, exAmpReadMode, exAmpGain, exRoi) =>
          variant.create.map: v =>
            Create(v, Common(exBin, exAmpReadMode, exAmpGain, exRoi))

    val NorthBinding: Matcher[North] =
      binding(GmosImagingFilterInput.NorthBinding)


    val SouthBinding: Matcher[South] =
      binding(GmosImagingFilterInput.SouthBinding)

  end Create

  // Edit ---------------------------------------------------------------------

  case class Edit[L](
    variant: Option[GmosImagingVariantInput[L]],
    common:  Edit.Common
  ):
    def toCreate: Result[Create[L]] =
      for
        v0 <- Result.fromOption(variant, OdbError.InvalidArgument("At least one filter must be specified for GMOS imaging observations".some).asProblem)
        v1 <- v0.create
      yield Create(v1, common.toCreate)

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
      val VariantBinding = GmosImagingVariantInput.binding(FilterBinding)
      ObjectFieldsBinding.rmap:
        case List(
          VariantBinding.Option("variant", rVariant),
          GmosBinningBinding.Nullable("explicitBin", rExplicitBin),
          GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi)
        ) => (
          rVariant,
          rExplicitBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
        ).parTupled.map: (variant, exBin, exAmpReadMode, exAmpGain, exRoi) =>
          Edit(variant, Common(exBin, exAmpReadMode, exAmpGain, exRoi))

    val NorthBinding: Matcher[North] =
      binding(GmosImagingFilterInput.NorthBinding)

    val SouthBinding: Matcher[South] =
      binding(GmosImagingFilterInput.SouthBinding)

  end Edit

end GmosImagingInput