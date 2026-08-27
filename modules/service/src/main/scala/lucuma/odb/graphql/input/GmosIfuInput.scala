// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Eq
import cats.data.NonEmptyList
import cats.derived.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthIfuFpu
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthIfuFpu
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.data.Nullable
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.binding.*

/**
 * Create and edit inputs for the GMOS North/South IFU observing mode.
 *
 * The shape follows GMOS MOS: the aperture is a builtin IFU rather than a custom mask, and the
 * telescope configurations are a plain list because the IFU dithers within its field rather than
 * nodding along a slit.  What IFU adds is the sampling geometry the ITC integrates over.
 */
object GmosIfuInput extends AcquisitionFilterCheck:

  final case class NorthAcquisition(
    filter:           Nullable[GmosNorthFilter],
    exposureTimeMode: Option[ExposureTimeMode]
  ):
    def updatesAcquisition: Boolean =
      filter.isDefined || exposureTimeMode.isDefined

  object NorthAcquisition:
    val Binding: Matcher[NorthAcquisition] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosNorthFilterBinding.Nullable("explicitFilter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode)
        ) => (
          acquisitionFilter(GmosNorthFilter.acquisition, rFilter),
          rExposureTimeMode
        ).parMapN(apply)

  final case class SouthAcquisition(
    filter:           Nullable[GmosSouthFilter],
    exposureTimeMode: Option[ExposureTimeMode]
  ):
    def updatesAcquisition: Boolean =
      filter.isDefined || exposureTimeMode.isDefined

  object SouthAcquisition:
    val Binding: Matcher[SouthAcquisition] =
      ObjectFieldsBinding.rmap:
        case List(
          GmosSouthFilterBinding.Nullable("explicitFilter", rFilter),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode)
        ) => (
          acquisitionFilter(GmosSouthFilter.acquisition, rFilter),
          rExposureTimeMode
        ).parMapN(apply)

  sealed trait Create[G, F, U]:
    def grating: G
    def filter:  Option[F]
    def fpu:     U
    def common:  Create.Common

  object Create:

    final case class Common(
      centralWavelength:        Wavelength,
      exposureTimeMode:         Option[ExposureTimeMode],
      explicitIfuAnalysis:      Option[GmosIfuAnalysis],
      explicitXBin:             Option[GmosXBinning],
      explicitYBin:             Option[GmosYBinning],
      explicitAmpReadMode:      Option[GmosAmpReadMode],
      explicitAmpGain:          Option[GmosAmpGain],
      explicitRoi:              Option[GmosRoi],
      explicitLambdaDithers:    Option[List[WavelengthDither]],
      explicitTelescopeConfigs: Option[NonEmptyList[TelescopeConfig]]
    ):

      // Formatted to store in a text column in the database with a regex constraint
      val formattedLambdaDithers: Option[String] =
        explicitLambdaDithers.map(GmosLongSlitInput.WavelengthDithersFormat.reverseGet)

      val formattedTelescopeConfigs: Option[String] =
        explicitTelescopeConfigs.map(ToSkyFormat.reverseGet)

    final case class North(
      grating:     GmosNorthGrating,
      filter:      Option[GmosNorthFilter],
      fpu:         GmosNorthIfuFpu,
      acquisition: Option[NorthAcquisition],
      common:      Common
    ) extends Create[GmosNorthGrating, GmosNorthFilter, GmosNorthIfuFpu]:
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthIfu

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (grating, filter, fpu, acquisition, common) =>
            Edit.North(grating, filter, fpu, acquisition, common).toCreate

    final case class South(
      grating:     GmosSouthGrating,
      filter:      Option[GmosSouthFilter],
      fpu:         GmosSouthIfuFpu,
      acquisition: Option[SouthAcquisition],
      common:      Common
    ) extends Create[GmosSouthGrating, GmosSouthFilter, GmosSouthIfuFpu]:
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthIfu

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (grating, filter, fpu, acquisition, common) =>
            Edit.South(grating, filter, fpu, acquisition, common).toCreate

  object Edit:

    final case class Common(
      centralWavelength:        Option[Wavelength],
      exposureTimeMode:         Option[ExposureTimeMode],
      explicitIfuAnalysis:      Nullable[GmosIfuAnalysis],
      explicitXBin:             Nullable[GmosXBinning],
      explicitYBin:             Nullable[GmosYBinning],
      explicitAmpReadMode:      Nullable[GmosAmpReadMode],
      explicitAmpGain:          Nullable[GmosAmpGain],
      explicitRoi:              Nullable[GmosRoi],
      explicitLambdaDithers:    Nullable[List[WavelengthDither]],
      explicitTelescopeConfigs: Nullable[NonEmptyList[TelescopeConfig]]
    ) derives Eq:

      def toCreate(site: Site): Result[Create.Common] =
        required(site, centralWavelength, "centralWavelength").map: w =>
          Create.Common(
            w,
            exposureTimeMode,
            explicitIfuAnalysis.toOption,
            explicitXBin.toOption,
            explicitYBin.toOption,
            explicitAmpReadMode.toOption,
            explicitAmpGain.toOption,
            explicitRoi.toOption,
            explicitLambdaDithers.toOption,
            explicitTelescopeConfigs.toOption
          )

      // Formatted to store in a text column in the database with a regex constraint
      val formattedLambdaDithers: Nullable[String] =
        explicitLambdaDithers.map(GmosLongSlitInput.WavelengthDithersFormat.reverseGet)

      val formattedTelescopeConfigs: Nullable[String] =
        explicitTelescopeConfigs.map(ToSkyFormat.reverseGet)

    object Common:
      val AllUndefined: Common =
        Common(None, None, Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent,
               Nullable.Absent, Nullable.Absent, Nullable.Absent, Nullable.Absent)

    private def required[A](site: Site, oa: Option[A], itemName: String): Result[A] =
      val siteName = site match
        case Site.GN => "North"
        case Site.GS => "South"

      Result.fromOption(oa, Matcher.validationProblem(s"A $itemName is required in order to create a GMOS $siteName IFU observing mode."))

    final case class North(
      grating:     Option[GmosNorthGrating],
      filter:      Nullable[GmosNorthFilter],
      fpu:         Option[GmosNorthIfuFpu],
      acquisition: Option[NorthAcquisition],
      common:      Edit.Common
    ) derives Eq:

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthIfu

      val toCreate: Result[Create.North] =
        for
          g <- required(Site.GN, grating, "grating")
          u <- required(Site.GN, fpu, "fpu")
          c <- common.toCreate(Site.GN)
        yield Create.North(g, filter.toOption, u, acquisition, c)

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (grating, filter, fpu, acquisition, common) =>
            Result(North(grating, filter, fpu, acquisition, common))

    final case class South(
      grating:     Option[GmosSouthGrating],
      filter:      Nullable[GmosSouthFilter],
      fpu:         Option[GmosSouthIfuFpu],
      acquisition: Option[SouthAcquisition],
      common:      Edit.Common
    ) derives Eq:

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthIfu

      val toCreate: Result[Create.South] =
        for
          g <- required(Site.GS, grating, "grating")
          u <- required(Site.GS, fpu, "fpu")
          c <- common.toCreate(Site.GS)
        yield Create.South(g, filter.toOption, u, acquisition, c)

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (grating, filter, fpu, acquisition, common) =>
            Result(South(grating, filter, fpu, acquisition, common))

  private val NorthData: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthIfuFpu],
    Option[NorthAcquisition],
    Edit.Common
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthIfuFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        GmosIfuAnalysisInput.Binding.Nullable("explicitIfuAnalysis", rExplicitIfuAnalysis),
        GmosBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        TelescopeConfigInput.Binding.List.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
        NorthAcquisition.Binding.Option("acquisition", rAcquisition)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rAcquisition,
        (
          rCentralWavelength,
          rExposureTimeMode,
          rExplicitIfuAnalysis,
          rExplicitXBin.map(_.map(GmosXBinning(_))),
          rExplicitYBin.map(_.map(GmosYBinning(_))),
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
          rWavelengthDithers,
          rTelescopeConfigs.flatMap(_.traverse: cs =>
            NonEmptyList.fromList(cs).fold(
              Matcher.validationFailure("'explicitTelescopeConfigs' must not be empty")
            )(Result(_))
          )
        ).parMapN(Edit.Common.apply)
      ).parTupled

  private val SouthData: Matcher[(
    Option[GmosSouthGrating],
    Nullable[GmosSouthFilter],
    Option[GmosSouthIfuFpu],
    Option[SouthAcquisition],
    Edit.Common
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthGratingBinding.Option("grating", rGrating),
        GmosSouthFilterBinding.Nullable("filter", rFilter),
        GmosSouthIfuFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        GmosIfuAnalysisInput.Binding.Nullable("explicitIfuAnalysis", rExplicitIfuAnalysis),
        GmosBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        TelescopeConfigInput.Binding.List.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
        SouthAcquisition.Binding.Option("acquisition", rAcquisition)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rAcquisition,
        (
          rCentralWavelength,
          rExposureTimeMode,
          rExplicitIfuAnalysis,
          rExplicitXBin.map(_.map(GmosXBinning(_))),
          rExplicitYBin.map(_.map(GmosYBinning(_))),
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
          rWavelengthDithers,
          rTelescopeConfigs.flatMap(_.traverse: cs =>
            NonEmptyList.fromList(cs).fold(
              Matcher.validationFailure("'explicitTelescopeConfigs' must not be empty")
            )(Result(_))
          )
        ).parMapN(Edit.Common.apply)
      ).parTupled

