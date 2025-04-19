// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import lucuma.core.enums.Instrument
import lucuma.core.model.ProgramReference
import lucuma.odb.graphql.binding.DescriptionBinding
import lucuma.odb.graphql.binding.InstrumentBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ProgramReferencePropertiesLibraryInput(
  instrument: Instrument,
  description: ProgramReference.Description
)

object ProgramReferencePropertiesLibraryInput {

  val Binding: Matcher[ProgramReferencePropertiesLibraryInput] =
    ObjectFieldsBinding.rmap {
      case List(
        InstrumentBinding("instrument", rInstrument),
        DescriptionBinding("description", rDescription)
      ) => (rInstrument, rDescription).parMapN(apply)
    }

}
