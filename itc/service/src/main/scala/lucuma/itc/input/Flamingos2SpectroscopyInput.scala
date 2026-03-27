// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput

case class Flamingos2SpectroscopyInput(
  exposureTimeMode: ExposureTimeMode,
  disperser:        Flamingos2Disperser,
  filter:           Flamingos2Filter,
  fpu:              Flamingos2Fpu,
  port:             PortDisposition
) extends InstrumentModesInput

object Flamingos2SpectroscopyInput:

  def binding: Matcher[Flamingos2SpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            Flamingos2DisperserBinding("disperser", disperser),
            Flamingos2FpuBinding("fpu", fpu),
            Flamingos2FilterBinding("filter", filter),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode, disperser, filter, fpu, portDisposition).parMapN(apply)
    }
