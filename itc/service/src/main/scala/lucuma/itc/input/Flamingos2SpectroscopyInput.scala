// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.PortDisposition
import lucuma.odb.graphql.binding.*

case class Flamingos2SpectroscopyInput(
  disperser: Flamingos2Disperser,
  filter:    Flamingos2Filter,
  fpu:       Flamingos2Fpu,
  port:      PortDisposition
) extends InstrumentModesInput

object Flamingos2SpectroscopyInput:

  def binding: Matcher[Flamingos2SpectroscopyInput] =
    ObjectFieldsBinding.rmap {
      case List(
            Flamingos2DisperserBinding("disperser", disperser),
            Flamingos2FpuBinding("fpu", fpu),
            Flamingos2FilterBinding("filter", filter),
            PortDispositionBinding("port", portDisposition)
          ) =>
        (disperser, filter, fpu, portDisposition).parMapN((d, f, u, p) =>
          Flamingos2SpectroscopyInput(d, f, u, p)
        )
    }
