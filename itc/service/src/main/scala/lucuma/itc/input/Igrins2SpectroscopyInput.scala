// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput

case class Igrins2SpectroscopyInput(
  exposureTimeMode: ExposureTimeMode
) extends InstrumentModesInput

object Igrins2SpectroscopyInput:
  val binding: Matcher[Igrins2SpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode)
          ) =>
        exposureTimeMode.map(Igrins2SpectroscopyInput.apply)
