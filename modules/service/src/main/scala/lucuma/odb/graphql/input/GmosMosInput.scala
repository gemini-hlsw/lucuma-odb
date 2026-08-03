// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Eq
import cats.derived.*
import cats.syntax.parallel.*
import grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

/**
 * Create and edit inputs for the GMOS North/South MOS observing mode.
 *
 * The shape follows GMOS Long Slit, with the builtin FPU replaced by a custom
 * mask and with no acquisition configuration.
 */
object GmosMosInput:

  sealed trait Create[G, F]:
    def grating:    G
    def filter:     Option[F]
    def customMask: GmosFpuMask.Custom
    def common:     Create.Common

  object Create:

    final case class Common(
      centralWavelength:   Wavelength,
      exposureTimeMode:    Option[ExposureTimeMode],
      explicitXBin:        Option[GmosXBinning],
      explicitYBin:        Option[GmosYBinning],
      explicitAmpReadMode: Option[GmosAmpReadMode],
      explicitAmpGain:     Option[GmosAmpGain],
      explicitRoi:         Option[GmosRoi],
      explicitλDithers:    Option[List[WavelengthDither]],
      explicitOffsets:     Option[List[Q]]
    ):

      // Formatted to store in a text column in the database with a regex constraint
      val formattedλDithers: Option[String] =
        explicitλDithers.map(GmosLongSlitInput.WavelengthDithersFormat.reverseGet)

      val formattedOffsets: Option[String] =
        explicitOffsets.map(GmosLongSlitInput.SpatialOffsetsFormat.reverseGet)

    final case class North(
      grating:    GmosNorthGrating,
      filter:     Option[GmosNorthFilter],
      customMask: GmosFpuMask.Custom,
      common:     Common
    ) extends Create[GmosNorthGrating, GmosNorthFilter]:
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthMos

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (grating, filter, customMask, common) =>
            Edit.North(grating, filter, customMask, common).toCreate

    final case class South(
      grating:    GmosSouthGrating,
      filter:     Option[GmosSouthFilter],
      customMask: GmosFpuMask.Custom,
      common:     Common
    ) extends Create[GmosSouthGrating, GmosSouthFilter]:
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthMos

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (grating, filter, customMask, common) =>
            Edit.South(grating, filter, customMask, common).toCreate

  object Edit:

    final case class Common(
      centralWavelength:   Option[Wavelength],
      exposureTimeMode:    Option[ExposureTimeMode],
      explicitXBin:        Nullable[GmosXBinning],
      explicitYBin:        Nullable[GmosYBinning],
      explicitAmpReadMode: Nullable[GmosAmpReadMode],
      explicitAmpGain:     Nullable[GmosAmpGain],
      explicitRoi:         Nullable[GmosRoi],
      explicitλDithers:    Nullable[List[WavelengthDither]],
      explicitOffsets:     Nullable[List[Q]]
    ) derives Eq:

      def toCreate(site: Site): Result[Create.Common] =
        required(site, centralWavelength, "centralWavelength").map: w =>
          Create.Common(
            w,
            exposureTimeMode,
            explicitXBin.toOption,
            explicitYBin.toOption,
            explicitAmpReadMode.toOption,
            explicitAmpGain.toOption,
            explicitRoi.toOption,
            explicitλDithers.toOption,
            explicitOffsets.toOption
          )

      // Formatted to store in a text column in the database with a regex constraint
      val formattedλDithers: Nullable[String] =
        explicitλDithers.map(GmosLongSlitInput.WavelengthDithersFormat.reverseGet)

      val formattedOffsets: Nullable[String] =
        explicitOffsets.map(GmosLongSlitInput.SpatialOffsetsFormat.reverseGet)

    object Common:
      val AllUndefined: Common =
        Common(None, None, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent)

    private def required[A](site: Site, oa: Option[A], itemName: String): Result[A] =
      val siteName = site match
        case Site.GN => "North"
        case Site.GS => "South"

      Result.fromOption(oa, Matcher.validationProblem(s"A $itemName is required in order to create a GMOS $siteName MOS observing mode."))

    final case class North(
      grating:    Option[GmosNorthGrating],
      filter:     Nullable[GmosNorthFilter],
      customMask: Option[GmosFpuMask.Custom],
      common:     Edit.Common
    ) derives Eq:

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthMos

      val toCreate: Result[Create.North] =
        for
          g <- required(Site.GN, grating, "grating")
          m <- required(Site.GN, customMask, "customMask")
          c <- common.toCreate(Site.GN)
        yield Create.North(g, filter.toOption, m, c)

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (grating, filter, customMask, common) =>
            Result(North(grating, filter, customMask, common))

    final case class South(
      grating:    Option[GmosSouthGrating],
      filter:     Nullable[GmosSouthFilter],
      customMask: Option[GmosFpuMask.Custom],
      common:     Edit.Common
    ) derives Eq:

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthMos

      val toCreate: Result[Create.South] =
        for
          g <- required(Site.GS, grating, "grating")
          m <- required(Site.GS, customMask, "customMask")
          c <- common.toCreate(Site.GS)
        yield Create.South(g, filter.toOption, m, c)

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (grating, filter, customMask, common) =>
            Result(South(grating, filter, customMask, common))

  private val NorthData: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosFpuMask.Custom],
    Edit.Common
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosCustomMaskInput.Binding.Option("customMask", rCustomMask),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        GmosBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        OffsetComponentInput.BindingQ.List.Nullable("explicitOffsets", rOffsets)
      ) => (
        rGrating,
        rFilter,
        rCustomMask,
        (
          rCentralWavelength,
          rExposureTimeMode,
          rExplicitXBin.map(_.map(GmosXBinning(_))),
          rExplicitYBin.map(_.map(GmosYBinning(_))),
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
          rWavelengthDithers,
          rOffsets
        ).parMapN(Edit.Common.apply)
      ).parTupled

  private val SouthData: Matcher[(
    Option[GmosSouthGrating],
    Nullable[GmosSouthFilter],
    Option[GmosFpuMask.Custom],
    Edit.Common
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthGratingBinding.Option("grating", rGrating),
        GmosSouthFilterBinding.Nullable("filter", rFilter),
        GmosCustomMaskInput.Binding.Option("customMask", rCustomMask),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        GmosBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        OffsetComponentInput.BindingQ.List.Nullable("explicitOffsets", rOffsets)
      ) => (
        rGrating,
        rFilter,
        rCustomMask,
        (
          rCentralWavelength,
          rExposureTimeMode,
          rExplicitXBin.map(_.map(GmosXBinning(_))),
          rExplicitYBin.map(_.map(GmosYBinning(_))),
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
          rWavelengthDithers,
          rOffsets
        ).parMapN(Edit.Common.apply)
      ).parTupled
