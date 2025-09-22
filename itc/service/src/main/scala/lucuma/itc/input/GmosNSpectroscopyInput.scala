// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.*

case class GmosNSpectroscopyInput(
  centralWavelength: Wavelength,
  grating:           GmosNorthGrating,
  fpu:               GmosFpuMask[GmosNorthFpu],
  filter:            Option[GmosNorthFilter],
  ccdMode:           Option[GmosCcdMode],
  roi:               Option[GmosRoi]
) extends InstrumentModesInput

object GmosNSpectroscopyInput {

  def binding: Matcher[GmosNSpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            WavelengthInput.Binding("centralWavelength", centralWavelength),
            GmosNorthGratingBinding("grating", grating),
            GmosNorthFpuInput.Binding("fpu", fpu),
            GmosNorthFilterBinding.Option("filter", filter),
            GmosCcdModeInput.Binding.Option("ccdMode", ccdMode),
            GmosRoiBinding.Option("roi", roi)
          ) =>
        (centralWavelength, grating, fpu, filter, ccdMode, roi).parMapN(apply)
    }

}
