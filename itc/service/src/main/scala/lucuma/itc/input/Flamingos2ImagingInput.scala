// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.PortDisposition
import lucuma.core.model.ExposureTimeMode
import lucuma.itc.binding.*
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.ExposureTimeModeInput

case class Flamingos2ImagingInput(
  exposureTimeMode: ExposureTimeMode,
  filter:           Flamingos2Filter,
  readMode:         Flamingos2ReadMode,
  port:             PortDisposition
) extends InstrumentModesInput

object Flamingos2ImagingInput:
  val binding: Matcher[Flamingos2ImagingInput] =
    ObjectFieldsBinding.rmap:
      case List(
            ExposureTimeModeInput.Binding("exposureTimeMode", exposureTimeMode),
            Flamingos2FilterBinding("filter", filter),
            Flamingos2ReadModeBinding("readMode", readMode),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (exposureTimeMode, filter, readMode, portDisposition).parMapN(apply)
