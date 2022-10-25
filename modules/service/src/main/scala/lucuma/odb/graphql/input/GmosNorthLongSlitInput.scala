// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

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
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Angle
import lucuma.core.math.Offset.Q
import lucuma.core.math.Wavelength
import lucuma.core.math.units.Nanometer
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.binding.*

import scala.util.control.Exception.*

object GmosNorthLongSlitInput {

  final case class Create(
    grating:                GmosNorthGrating,
    filter:                 Option[GmosNorthFilter],
    fpu:                    GmosNorthFpu,
    centralWavelength:      Wavelength,
    explicitXBin:           Option[GmosXBinning],
    explicitYBin:           Option[GmosYBinning],
    explicitAmpReadMode:    Option[GmosAmpReadMode],
    explicitAmpGain:        Option[GmosAmpGain],
    explicitRoi:            Option[GmosRoi],
    explicitλDithers:       Option[List[BigDecimal]],
    explicitSpatialOffsets: Option[List[Q]]
  ) {
    
    def observingModeType: ObservingModeType =
      ObservingModeType.GmosNorthLongSlit

    // Formatted to store in a text column in the database with a regex constraint
    val formattedλDithers: Option[String] =
      explicitλDithers.map(_.map(_.bigDecimal.toPlainString).intercalate(","))

    val formattedSpatialOffsets: Option[String] =
      explicitSpatialOffsets.map(_.map(q => Angle.signedDecimalArcseconds.get(q.toAngle).bigDecimal.toPlainString).intercalate(","))

  }

  final case class Edit(
    grating:                Option[GmosNorthGrating],
    filter:                 Nullable[GmosNorthFilter],
    fpu:                    Option[GmosNorthFpu],
    centralWavelength:      Option[Wavelength],
    explicitXBin:           Nullable[GmosXBinning],
    explicitYBin:           Nullable[GmosYBinning],
    explicitAmpReadMode:    Nullable[GmosAmpReadMode],
    explicitAmpGain:        Nullable[GmosAmpGain],
    explicitRoi:            Nullable[GmosRoi],
    explicitλDithers:       Nullable[List[BigDecimal]],
    explicitSpatialOffsets: Nullable[List[Q]]
  ) {
    
    val observingModeType: ObservingModeType =
      ObservingModeType.GmosNorthLongSlit

    val toCreate: Result[Create] = {
      def required[A](oa: Option[A], itemName: String): Result[A] =
        Result.fromOption(oa, s"A $itemName is required in order to create a GMOS North Long Slit observing mode.")

      for {
        g <- required(grating, "grating")
        u <- required(fpu, "fpu")
        w <- required(centralWavelength, "centralWavelength")
      } yield Create(
        g,
        filter.toOption,
        u,
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

  }

  private val data: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthFpu],
    Option[Wavelength],
    Nullable[GmosXBinning],
    Nullable[GmosYBinning],
    Nullable[GmosAmpReadMode],
    Nullable[GmosAmpGain],
    Nullable[GmosRoi],
    Nullable[List[BigDecimal]],
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
        BigDecimalBinding.List.Nullable("explicitWavelengthDithersNm", rWavelengthDithers),
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

  val CreateBinding: Matcher[Create] =
    data.rmap {
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
        Result(Create(
          grating,
          filter.toOption,
          fpu,
          centralWavelength,
          exXBin.toOption,
          exYBin.toOption,
          exAmpReadMode.toOption,
          exAmpGain.toOption,
          exRoi.toOption,
          exWavelengthDithers.toOption,
          exSpatialOffsets.toOption
        ))
      case _ =>
        Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")
    }

  val EditBinding: Matcher[Edit] =
    data.rmap {
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
        Result(Edit(
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
        ))

    }
}

