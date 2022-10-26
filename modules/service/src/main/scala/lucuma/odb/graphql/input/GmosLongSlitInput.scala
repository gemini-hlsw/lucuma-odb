// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import coulomb.Quantity
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Picometer
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.binding.*

import scala.util.control.Exception.*

object GmosLongSlitInput {

  object Create {

    final case class Common(
      centralWavelength:      Wavelength,
      explicitXBin:           Option[GmosXBinning],
      explicitYBin:           Option[GmosYBinning],
      explicitAmpReadMode:    Option[GmosAmpReadMode],
      explicitAmpGain:        Option[GmosAmpGain],
      explicitRoi:            Option[GmosRoi],
      explicitλDithers:       Option[List[Quantity[Int, Picometer]]],
      explicitSpatialOffsets: Option[List[Q]]
    ) {

      // Formatted to store in a text column in the database with a regex constraint
      val formattedλDithers: Option[String] =
        explicitλDithers.map(GmosLongSlitInput.formattedλDithers)

      val formattedSpatialOffsets: Option[String] =
        explicitSpatialOffsets.map(GmosLongSlitInput.formattedSpatialOffsets)

    }

    final case class North(
      grating: GmosNorthGrating,
      filter:  Option[GmosNorthFilter],
      fpu:     GmosNorthFpu,
      common:  Common
    ) {

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthLongSlit

    }

    object North {

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (
            Some(grating),
            filter,
            Some(fpu),
            Some(centralWavelength),
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(North(
              grating,
              filter.toOption,
              fpu,
              Common(
                centralWavelength,
                exXBin.toOption,
                exYBin.toOption,
                exAmpReadMode.toOption,
                exAmpGain.toOption,
                exRoi.toOption,
                exWavelengthDithers.toOption,
                exSpatialOffsets.toOption
              )
            ))
          case _ =>
            Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")
        }

    }

    final case class South(
      grating: GmosSouthGrating,
      filter:  Option[GmosSouthFilter],
      fpu:     GmosSouthFpu,
      common:  Common
    ) {

      def observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthLongSlit

    }

    object South {

      val Binding: Matcher[South] =
        SouthData.rmap {
          case (
            Some(grating),
            filter,
            Some(fpu),
            Some(centralWavelength),
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(South(
              grating,
              filter.toOption,
              fpu,
              Common(
                centralWavelength,
                exXBin.toOption,
                exYBin.toOption,
                exAmpReadMode.toOption,
                exAmpGain.toOption,
                exRoi.toOption,
                exWavelengthDithers.toOption,
                exSpatialOffsets.toOption
              )
            ))
          case _ =>
            Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS South Long Slit observing mode.")
        }

    }

  }


  object Edit {

    final case class Common(
      centralWavelength:      Option[Wavelength],
      explicitXBin:           Nullable[GmosXBinning],
      explicitYBin:           Nullable[GmosYBinning],
      explicitAmpReadMode:    Nullable[GmosAmpReadMode],
      explicitAmpGain:        Nullable[GmosAmpGain],
      explicitRoi:            Nullable[GmosRoi],
      explicitλDithers:       Nullable[List[Quantity[Int, Picometer]]],
      explicitSpatialOffsets: Nullable[List[Q]]
    ) {

      def toCreate: Result[Create.Common] =
        required(centralWavelength, "centralWavelength").map { w =>
          Create.Common(
            w,
            explicitXBin.toOption,
            explicitYBin.toOption,
            explicitAmpReadMode.toOption,
            explicitAmpGain.toOption,
            explicitRoi.toOption,
            explicitλDithers.toOption,
            explicitSpatialOffsets.toOption
          )
        }

      // Formatted to store in a text column in the database with a regex constraint
      val formattedλDithers: Nullable[String] =
        explicitλDithers.map(GmosLongSlitInput.formattedλDithers)

      val formattedSpatialOffsets: Nullable[String] =
        explicitSpatialOffsets.map(GmosLongSlitInput.formattedSpatialOffsets)

    }

    private def required[A](oa: Option[A], itemName: String): Result[A] =
      Result.fromOption(oa, s"A $itemName is required in order to create a GMOS North Long Slit observing mode.")

    final case class North(
      grating: Option[GmosNorthGrating],
      filter:  Nullable[GmosNorthFilter],
      fpu:     Option[GmosNorthFpu],
      common:  Edit.Common
    ) {

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosNorthLongSlit

      val toCreate: Result[Create.North] =
        for {
          g <- required(grating, "grating")
          u <- required(fpu, "fpu")
          c <- common.toCreate
        } yield Create.North(g, filter.toOption, u, c)

    }

    object North {

      val Binding: Matcher[North] =
        NorthData.rmap {
          case (
            grating,
            filter,
            fpu,
            centralWavelength,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(North(
              grating,
              filter,
              fpu,
              Common(
                centralWavelength,
                exXBin,
                exYBin,
                exAmpReadMode,
                exAmpGain,
                exRoi,
                exWavelengthDithers,
                exSpatialOffsets
              )
            ))

        }
    }
    
    final case class South(
      grating: Option[GmosSouthGrating],
      filter:  Nullable[GmosSouthFilter],
      fpu:     Option[GmosSouthFpu],
      common:  Edit.Common
    ) {

      val observingModeType: ObservingModeType =
        ObservingModeType.GmosSouthLongSlit

      val toCreate: Result[Create.South] =
        for {
          g <- required(grating, "grating")
          u <- required(fpu, "fpu")
          c <- common.toCreate
        } yield Create.South(g, filter.toOption, u, c)

    }

    object South {

      val Binding: Matcher[South] =
        SouthData.rmap {
          case (
            grating,
            filter,
            fpu,
            centralWavelength,
            exXBin,
            exYBin,
            exAmpReadMode,
            exAmpGain,
            exRoi,
            exWavelengthDithers,
            exSpatialOffsets
            ) =>
            Result(South(
              grating,
              filter,
              fpu,
              Common(
                centralWavelength,
                exXBin,
                exYBin,
                exAmpReadMode,
                exAmpGain,
                exRoi,
                exWavelengthDithers,
                exSpatialOffsets
              )
            ))

        }
    }    
  }

  private def formattedλDithers(in: List[Quantity[Int, Picometer]]): String =
    in.map(w => BigDecimal(w.value).bigDecimal.movePointLeft(3).toPlainString).intercalate(",")

  private def formattedSpatialOffsets(in: List[Q]): String =
    in.map(q => Angle.signedDecimalArcseconds.get(q.toAngle).bigDecimal.toPlainString).intercalate(",")

  private val NorthData: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthFpu],
    Option[Wavelength],
    Nullable[GmosXBinning],
    Nullable[GmosYBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
    Nullable[List[Quantity[Int, Picometer]]],
    Nullable[List[Q]]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        GmosXBinningBinding.Nullable("explicitXBin", rExplicitXBin),
        GmosYBinningBinding.Nullable("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
        WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
        OffsetComponentInput.Binding.List.Nullable("explicitSpatialOffsets", rSpatialOffsets)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rCentralWavelength,
        rExplicitXBin,
        rExplicitYBin,
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi,
        rWavelengthDithers,
        rSpatialOffsets
      ).parTupled
    }

  private val SouthData: Matcher[(
    Option[GmosSouthGrating],
      Nullable[GmosSouthFilter],
      Option[GmosSouthFpu],
      Option[Wavelength],
      Nullable[GmosXBinning],
      Nullable[GmosYBinning],
      Nullable[GmosAmpReadMode],
      Nullable[GmosAmpGain],
      Nullable[GmosRoi],
      Nullable[List[Quantity[Int, Picometer]]],
      Nullable[List[Q]]
    )] =
      ObjectFieldsBinding.rmap {
        case List(
          GmosSouthGratingBinding.Option("grating", rGrating),
          GmosSouthFilterBinding.Nullable("filter", rFilter),
          GmosSouthFpuBinding.Option("fpu", rFpu),
          WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
          GmosXBinningBinding.Nullable("explicitXBin", rExplicitXBin),
          GmosYBinningBinding.Nullable("explicitYBin", rExplicitYBin),
          GmosAmpReadModeBinding.Nullable("explicitAmpReadMode", rExplicitAmpReadMode),
          GmosAmpGainBinding.Nullable("explicitAmpGain", rExplicitAmpGain),
          GmosRoiBinding.Nullable("explicitRoi", rExplicitRoi),
          WavelengthDitherInput.Binding.List.Nullable("explicitWavelengthDithers", rWavelengthDithers),
          OffsetComponentInput.Binding.List.Nullable("explicitSpatialOffsets", rSpatialOffsets)
        ) => (
          rGrating,
          rFilter,
          rFpu,
          rCentralWavelength,
          rExplicitXBin,
          rExplicitYBin,
          rExplicitAmpReadMode,
          rExplicitAmpGain,
          rExplicitRoi,
          rWavelengthDithers,
          rSpatialOffsets
        ).parTupled
      }
}

