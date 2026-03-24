// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.model.ExposureTimeMode
import lucuma.core.enums.PortDisposition
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput

case class Igrins2SpectroscopyInput(
  exposureTimeMode: ExposureTimeMode,
  port:             PortDisposition
) extends InstrumentModesInput

object Igrins2SpectroscopyInput:
  val binding: Matcher[Igrins2SpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode, portDisposition).parMapN(apply)
