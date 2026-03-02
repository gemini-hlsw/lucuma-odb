// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.input

import cats.syntax.functor.*
import lucuma.odb.graphql.binding.*

case class Igrins2SpectroscopyInput() extends InstrumentModesInput

object Igrins2SpectroscopyInput:
  val binding: Matcher[Igrins2SpectroscopyInput] =
    ObjectFieldsBinding.rmap:
      case List(
            EnumBinding.Option("ignore", rIgnore)
          ) =>
        rIgnore.as(Igrins2SpectroscopyInput())
