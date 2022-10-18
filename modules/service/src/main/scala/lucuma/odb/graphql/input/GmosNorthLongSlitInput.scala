// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.binding.*


object GmosNorthLongSlitInput {

  final case class Create(
    grating:             GmosNorthGrating,
    filter:              Option[GmosNorthFilter],
    fpu:                 GmosNorthFpu,
    centralWavelength:   Wavelength,
    explicitXBin:        Option[GmosXBinning],
    explicitYBin:        Option[GmosYBinning],
    explicitAmpReadMode: Option[GmosAmpReadMode],
    explicitAmpGain:     Option[GmosAmpGain],
    explicitRoi:         Option[GmosRoi]
  ) {
    
    def observingModeType: ObservingModeType =
      ObservingModeType.GmosNorthLongSlit
    
  }

  private val data: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthFpu],
    Option[Wavelength],
    Option[GmosXBinning],
    Option[GmosYBinning],
    Option[GmosAmpReadMode],
    Option[GmosAmpGain],
    Option[GmosRoi]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        GmosXBinningBinding.Option("explicitXBin", rExplicitXBin),
        GmosYBinningBinding.Option("explicitYBin", rExplicitYBin),
        GmosAmpReadModeBinding.Option("explicitAmpReadMode", rExplicitAmpReadMode),
        GmosAmpGainBinding.Option("explicitAmpGain", rExplicitAmpGain),
        GmosRoiBinding.Option("explicitRoi", rExplicitRoi),
        ("explicitWavelengthDithersNm", _),
        ("explicitSpatialOffsets", _)
      ) => (
        rGrating,
        rFilter,
        rFpu,
        rCentralWavelength,
        rExplicitXBin,
        rExplicitYBin,
        rExplicitAmpReadMode,
        rExplicitAmpGain,
        rExplicitRoi
      ).parTupled
    }

  val CreateBinding: Matcher[Create] =
    data.rmap {
      case (Some(grating), filter, Some(fpu), Some(centralWavelength), explicitXBin, explicitYBin, explicitAmpReadMode, explicitAmpGain, explicitRoi) =>
        Result(Create(grating, filter.toOption, fpu, centralWavelength, explicitXBin, explicitYBin, explicitAmpReadMode, explicitAmpGain, explicitRoi))
      case _                                                                         =>
        Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")
    }

}

