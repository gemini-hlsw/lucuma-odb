// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import lucuma.odb.graphql.binding.*

case class Igrins2SpectroscopyInput(
  _placeholder: Boolean = false
) extends InstrumentModesInput

object Igrins2SpectroscopyInput:
  val binding: Matcher[Igrins2SpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            BooleanBinding.Option("_placeholder", placeholder)
          ) =>
        placeholder.map(p => Igrins2SpectroscopyInput(p.getOrElse(false)))
