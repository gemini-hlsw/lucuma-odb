// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.foldable.*
import cats.syntax.option.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import coulomb.Quantity
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosLongSlitAcquisitionRoi
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.Site
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDither
import lucuma.core.model.ExposureTimeMode
import lucuma.core.optics.Format
import lucuma.core.syntax.string.*
import lucuma.odb.data.Nullable
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.format.spatialOffsets.*
import lucuma.odb.graphql.binding.*

import scala.util.Try

object GmosLongSlitInput:

  val WavelengthDithersFormat: Format[String, List[WavelengthDither]] =
    Format(
      s =>
        for
          ns <- Try(s.split(",").toList.map(BigDecimal.exact)).toOption
          ws <- ns.traverse(WavelengthDither.decimalNanometers.getOption)
        yield ws,
      _.map(_.toNanometers.value.bigDecimal.toPlainString).intercalate(",")
    )

  val SpatialOffsetsFormat: Format[String, List[Q]] = OffsetsQFormat

  final case class NorthAcquisition(
    filter:           Nullable[GmosNorthFilter],
    roi:              Nullable[GmosLongSlitAcquisitionRoi],
    exposureTimeMode: Option[ExposureTimeMode]
  )

  object NorthAcquisition:
    val Binding: Matcher[NorthAcquisition] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosNorthFilterBinding.Nullable("explicitFilter", rFilter),
          GmosLongSlitAcquisitionRoiBinding.Nullable("explicitRoi", rRoi),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode)
        ) => (
          rFilter.flatMap: n =>
            n.traverse: f =>
              if GmosNorthFilter.acquisition.toList.contains(f) then f.success
              else OdbError.InvalidArgument(s"'explicitFilter' must contain one of: ${GmosNorthFilter.acquisition.map(_.tag.toScreamingSnakeCase).mkString_(", ")}".some).asFailure
          ,
          rRoi,
          rExposureTimeMode
        ).parMapN(apply)
      }

  final case class SouthAcquisition(
    filter:           Nullable[GmosSouthFilter],
    roi:              Nullable[GmosLongSlitAcquisitionRoi],
    exposureTimeMode: Option[ExposureTimeMode]
  )

  object SouthAcquisition:
    val Binding: Matcher[SouthAcquisition] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosSouthFilterBinding.Nullable("explicitFilter", rFilter),
          GmosLongSlitAcquisitionRoiBinding.Nullable("explicitRoi", rRoi),
          ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode)
        ) => (
          rFilter.flatMap: n =>
            n.traverse: f =>
              if GmosSouthFilter.acquisition.toList.contains(f) then f.success
              else OdbError.InvalidArgument(s"'explicitFilter' must contain one of: ${GmosSouthFilter.acquisition.map(_.tag.toScreamingSnakeCase).mkString_(", ")}".some).asFailure
          ,
          rRoi,
          rExposureTimeMode
        ).parMapN(apply)
      }

  sealed trait Create[G, F, U]:
    def grating: G
    def filter:  Option[F]
    def fpu:     U
    def common:  Create.Common

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
        explicitλDithers.map(WavelengthDithersFormat.reverseGet)

      val formattedOffsets: Option[String] =
        explicitOffsets.map(SpatialOffsetsFormat.reverseGet)

    final case class North(
      grating:     GmosNorthGrating,
      filter:      Option[GmosNorthFilter],
      fpu:         GmosNorthFpu,
      common:      Common,
      acquisition: Option[NorthAcquisition]
    ) extends Create[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]:
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthLongSlit

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (
            Some(grating),
            filter,
            Some(fpu),
            Some(centralWavelength),
            exposureTimeMode,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exOffsets,
            acquisition
          ) =>
            Result(North(
              grating,
              filter.toOption,
              fpu,
              Common(
                centralWavelength,
                exposureTimeMode,
                exXBin.toOption,
                exYBin.toOption,
                exAmpReadMode.toOption,
                exAmpGain.toOption,
                exRoi.toOption,
                exWavelengthDithers.toOption,
                exOffsets.toOption
              ),
              acquisition
            ))
          case _ =>
            Matcher.validationFailure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")

    final case class South(
      grating:     GmosSouthGrating,
      filter:      Option[GmosSouthFilter],
      fpu:         GmosSouthFpu,
      common:      Common,
      acquisition: Option[SouthAcquisition]
    ) extends Create[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]:
      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthLongSlit

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (
            Some(grating),
            filter,
            Some(fpu),
            Some(centralWavelength),
            exposureTimeMode,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exOffsets,
            acquisition
          ) =>
            Result(South(
              grating,
              filter.toOption,
              fpu,
              Common(
                centralWavelength,
                exposureTimeMode,
                exXBin.toOption,
                exYBin.toOption,
                exAmpReadMode.toOption,
                exAmpGain.toOption,
                exRoi.toOption,
                exWavelengthDithers.toOption,
                exOffsets.toOption
              ),
              acquisition
            ))
          case _ =>
            Matcher.validationFailure("grating, fpu, and centralWavelength are required when creating the GMOS South Long Slit observing mode.")


  object Edit:

    final case class Common(
      centralWavelength:    Option[Wavelength],
      exposureTimeMode:     Option[ExposureTimeMode],
      explicitXBin:         Nullable[GmosXBinning],
      explicitYBin:         Nullable[GmosYBinning],
      explicitAmpReadMode:  Nullable[GmosAmpReadMode],
      explicitAmpGain:      Nullable[GmosAmpGain],
      explicitRoi:          Nullable[GmosRoi],
      explicitλDithers:     Nullable[List[WavelengthDither]],
      explicitOffsets:      Nullable[List[Q]]
    ):

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
        explicitλDithers.map(WavelengthDithersFormat.reverseGet)

      val formattedOffsets: Nullable[String] =
        explicitOffsets.map(SpatialOffsetsFormat.reverseGet)

    private def required[A](site: Site, oa: Option[A], itemName: String): Result[A] =
      val siteName = site match
        case Site.GN => "North"
        case Site.GS => "South"

      Result.fromOption(oa, Matcher.validationProblem(s"A $itemName is required in order to create a GMOS ${siteName} Long Slit observing mode."))

    final case class North(
      grating:     Option[GmosNorthGrating],
      filter:      Nullable[GmosNorthFilter],
      fpu:         Option[GmosNorthFpu],
      common:      Edit.Common,
      acquisition: Option[NorthAcquisition]
    ):

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthLongSlit

      val toCreate: Result[Create.North] =
        for
          g <- required(Site.GN, grating, "grating")
          u <- required(Site.GN, fpu, "fpu")
          c <- common.toCreate(Site.GN)
        yield Create.North(g, filter.toOption, u, c, acquisition)

    object North:

      val Binding: Matcher[North] =
        NorthData.rmap:
          case (
            grating,
            filter,
            fpu,
            centralWavelength,
            exposureTimeMode,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exOffsets,
            acquisition
          ) =>
            Result(North(
              grating,
              filter,
              fpu,
              Common(
                centralWavelength,
                exposureTimeMode,
                exXBin,
                exYBin,
                exAmpReadMode,
                exAmpGain,
                exRoi,
                exWavelengthDithers,
                exOffsets
              ),
              acquisition
            ))

    final case class South(
      grating:     Option[GmosSouthGrating],
      filter:      Nullable[GmosSouthFilter],
      fpu:         Option[GmosSouthFpu],
      common:      Edit.Common,
      acquisition: Option[SouthAcquisition]
    ):

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthLongSlit

      val toCreate: Result[Create.South] =
        for
          g <- required(Site.GS, grating, "grating")
          u <- required(Site.GS, fpu, "fpu")
          c <- common.toCreate(Site.GS)
        yield Create.South(g, filter.toOption, u, c, acquisition)

    object South:

      val Binding: Matcher[South] =
        SouthData.rmap:
          case (
            grating,
            filter,
            fpu,
            centralWavelength,
            exposureTimeMode,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exOffsets,
            acquisition
          ) =>
            Result(South(
              grating,
              filter,
              fpu,
              Common(
                centralWavelength,
                exposureTimeMode,
                exXBin,
                exYBin,
                exAmpReadMode,
                exAmpGain,
                exRoi,
                exWavelengthDithers,
                exOffsets
              ),
              acquisition
            ))


  private val NorthData: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthFpu],
    Option[Wavelength],
    Option[ExposureTimeMode],
    Nullable[GmosXBinning],
    Nullable[GmosYBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
    Nullable[List[WavelengthDither]],
    Nullable[List[Q]],
    Option[NorthAcquisition]
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        GmosBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        OffsetComponentInput.BindingQ.List.Nullable("explicitOffsets", rOffsets),
        OffsetComponentInput.BindingQ.List.Nullable("explicitSpatialOffsets", rSpatialOffsets),
        NorthAcquisition.Binding.Option("acquisition", rAcquisition)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rCentralWavelength,
        rExposureTimeMode,
        rExplicitXBin.map(_.map(GmosXBinning(_))),
        rExplicitYBin.map(_.map(GmosYBinning(_))),
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi,
        rWavelengthDithers,
        (rOffsets, rSpatialOffsets).parMapN { (offsets, spatialOffsets) =>
          offsets.orElse(spatialOffsets)
        },
        rAcquisition
      ).parTupled

  private val SouthData: Matcher[(
    Option[GmosSouthGrating],
    Nullable[GmosSouthFilter],
    Option[GmosSouthFpu],
    Option[Wavelength],
    Option[ExposureTimeMode],
    Nullable[GmosXBinning],
    Nullable[GmosYBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
    Nullable[List[WavelengthDither]],
    Nullable[List[Q]],
    Option[SouthAcquisition]
  )] =
    ObjectFieldsBinding.rmap:
      case List(
        GmosSouthGratingBinding.Option("grating", rGrating),
        GmosSouthFilterBinding.Nullable("filter", rFilter),
        GmosSouthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ExposureTimeModeInput.Binding.Option("exposureTimeMode", rExposureTimeMode),
        GmosBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        OffsetComponentInput.BindingQ.List.Nullable("explicitOffsets", rOffsets),
        OffsetComponentInput.BindingQ.List.Nullable("explicitSpatialOffsets", rSpatialOffsets),
        SouthAcquisition.Binding.Option("acquisition", rAcquisition)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rCentralWavelength,
        rExposureTimeMode,
        rExplicitXBin.map(_.map(GmosXBinning(_))),
        rExplicitYBin.map(_.map(GmosYBinning(_))),
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi,
        rWavelengthDithers,
        (rOffsets, rSpatialOffsets).parMapN { (offsets, spatialOffsets) =>
          offsets.orElse(spatialOffsets)
        },
        rAcquisition
      ).parTupled