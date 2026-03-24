// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.PortDisposition
import lucuma.core.math.Wavelength
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*
import lucuma.odb.graphql.input.ExposureTimeModeInput

case class GmosNSpectroscopyInput(
  exposureTimeMode:  ExposureTimeMode,
  centralWavelength: Wavelength,
  grating:           GmosNorthGrating,
  fpu:               GmosFpuMask[GmosNorthFpu],
  filter:            Option[GmosNorthFilter],
  ccdMode:           Option[GmosCcdMode],
  roi:               Option[GmosRoi],
  port:              PortDisposition
) extends InstrumentModesInput

object GmosNSpectroscopyInput {

  def binding: Matcher[GmosNSpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            WavelengthInput.Binding("centralWavelength", centralWavelength),
            GmosNorthGratingBinding("grating", grating),
            GmosNorthFpuInput.Binding("fpu", fpu),
            GmosNorthFilterBinding.Option("filter", filter),
            GmosCcdModeInput.Binding.Option("ccdMode", ccdMode),
            GmosRoiBinding.Option("roi", roi),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode, centralWavelength, grating, fpu, filter, ccdMode, roi, portDisposition).parMapN(apply)
    }

}
