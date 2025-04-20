// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Instrument
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ProgramReferencePropertiesExampleInput(
  instrument: Instrument
)

object ProgramReferencePropertiesExampleInput {

  val Binding: Matcher[ProgramReferencePropertiesExampleInput] =
    ObjectFieldsBinding.rmap {
      case List(
        InstrumentBinding("instrument", rInstrument)
      ) => rInstrument.map(apply)
    }

}