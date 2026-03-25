// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.parallel.*
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.PortDisposition
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.odb.graphql.binding.*
import lucuma.odb.graphql.input.GmosCcdModeInput

case class GmosSImagingInput(
  filter:  GmosSouthFilter,
  ccdMode: Option[GmosCcdMode],
  port:    PortDisposition
) extends InstrumentModesInput

object GmosSImagingInput {

  def binding: Matcher[GmosSImagingInput] =
    ObjectFieldsBinding.rmap {
      case List(GmosSouthFilterBinding("filter", filter),
                GmosCcdModeInput.Binding.Option("ccdMode", ccdMode),
                PortDispositionBinding("port", portDisposition)
          ) =>
        (filter, ccdMode, portDisposition).parMapN(apply)
    }

}
