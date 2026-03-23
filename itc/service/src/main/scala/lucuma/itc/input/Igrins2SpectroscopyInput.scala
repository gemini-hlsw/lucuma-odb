// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import lucuma.core.enums.PortDisposition
import lucuma.odb.graphql.binding.*

case class Igrins2SpectroscopyInput(port: PortDisposition) extends InstrumentModesInput

object Igrins2SpectroscopyInput:
  val binding: Matcher[Igrins2SpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            PortDispositionBinding("port", portDisposition)
          ) =>
        portDisposition.map(Igrins2SpectroscopyInput.apply)
