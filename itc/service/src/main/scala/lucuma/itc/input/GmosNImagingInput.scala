// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput
import lucuma.odb.graphql.input.GmosCcdModeInput

case class GmosNImagingInput(
  exposureTimeMode: ExposureTimeMode,
  filter:           GmosNorthFilter,
  ccdMode:          Option[GmosCcdMode],
  port:             PortDisposition
) extends InstrumentModesInput

object GmosNImagingInput {

  def binding: Matcher[GmosNImagingInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            GmosNorthFilterBinding("filter", filter),
            GmosCcdModeInput.Binding.Option("ccdMode", ccdMode),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode, filter, ccdMode, portDisposition).parMapN(apply)
    }

}
