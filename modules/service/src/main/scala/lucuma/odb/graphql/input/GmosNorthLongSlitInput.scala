// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.flatMap.*
import cats.syntax.parallel.*
import edu.gemini.grackle.Result
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.math.Wavelength
import lucuma.odb.data.Nullable
import lucuma.odb.data.ObservingModeType
import lucuma.odb.graphql.binding.*


object GmosNorthLongSlitInput {

  final case class Create(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: Wavelength
  ) {
    
    def observingModeType: ObservingModeType =
      ObservingModeType.GmosNorthLongSlit
    
  }

  private val data: Matcher[(
    Option[GmosNorthGrating],
    Nullable[GmosNorthFilter],
    Option[GmosNorthFpu],
    Option[Wavelength]
  )] =
    ObjectFieldsBinding.rmap {
      case List(
        GmosNorthGratingBinding.Option("grating", rGrating),
        GmosNorthFilterBinding.Nullable("filter", rFilter),
        GmosNorthFpuBinding.Option("fpu", rFpu),
        WavelengthInput.Binding.Option("centralWavelength", rCentralWavelength),
        ("explicitXBin", _),
        ("explicitYBin", _),
        ("explicitAmpReadMode", _),
        ("explicitAmpGain", _),
        ("explicitRoi", _),
        ("explicitWavelengthDithersNm", _),
        ("explicitSpatialOffsets", _)
      ) => (rGrating, rFilter, rFpu, rCentralWavelength).parTupled
    }

  val CreateBinding: Matcher[Create] =
    data.rmap {
      case (Some(grating), filter, Some(fpu), Some(centralWavelength)) =>
        Result(Create(grating, filter.toOption, fpu, centralWavelength))
      case _                                                           =>
        Result.failure("grating, fpu, and centralWavelength are required when creating the GMOS North Long Slit observing mode.")
    }

}

