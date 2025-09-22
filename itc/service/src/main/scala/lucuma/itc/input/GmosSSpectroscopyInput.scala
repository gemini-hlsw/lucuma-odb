// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class GmosSSpectroscopyInput(
  centralWavelength: Wavelength,
  grating:           GmosSouthGrating,
  fpu:               GmosFpuMask[GmosSouthFpu],
  filter:            Option[GmosSouthFilter],
  ccdMode:           Option[GmosCcdMode],
  roi:               Option[GmosRoi]
) extends InstrumentModesInput

object GmosSSpectroscopyInput {

  def binding: Matcher[GmosSSpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("centralWavelength", centralWavelength),
            GmosSouthGratingBinding("grating", grating),
            GmosSouthFpuInput.Binding("fpu", fpu),
            GmosSouthFilterBinding.Option("filter", filter),
            GmosCcdModeInput.Binding.Option("ccdMode", ccdMode),
            GmosRoiBinding.Option("roi", roi)
          ) =>
        (centralWavelength, grating, fpu, filter, ccdMode, roi).parMapN(apply)
    }

}
