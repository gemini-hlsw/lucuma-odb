// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.odb.graphql.util.Bindings._

case class CreateProgramInput(
  SET: ProgramPropertiesInput
)

object CreateProgramInput {
  val Binding: Matcher[CreateProgramInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramPropertiesInput.Binding.Option("SET", rInput)
      ) => rInput.map(o => CreateProgramInput(o.getOrElse(ProgramPropertiesInput.Default)))
    }
}



