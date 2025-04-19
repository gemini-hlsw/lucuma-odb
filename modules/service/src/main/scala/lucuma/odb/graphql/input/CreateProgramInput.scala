// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.odb.graphql.binding.*

case class CreateProgramInput(
  SET: Option[ProgramPropertiesInput.Create]
)

object CreateProgramInput {
  val Binding: Matcher[CreateProgramInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramPropertiesInput.Create.Binding.Option("SET", rInput)
      ) => rInput.map(apply)
    }
}



