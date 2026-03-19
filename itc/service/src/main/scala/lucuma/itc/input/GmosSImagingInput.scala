// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput
import lucuma.odb.graphql.input.GmosCcdModeInput

case class GmosSImagingInput(
  exposureTimeMode: ExposureTimeMode,
  filter:           GmosSouthFilter,
  ccdMode:          Option[GmosCcdMode]
) extends InstrumentModesInput

object GmosSImagingInput {

  def binding: Matcher[GmosSImagingInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            GmosSouthFilterBinding("filter", filter),
            GmosCcdModeInput.Binding.Option("ccdMode", ccdMode)
          ) =>
        (exposureTimeMode, filter, ccdMode).parMapN(apply)
    }

}
