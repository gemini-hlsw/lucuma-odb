// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.odb.graphql.binding._

case class CreateProgramInput(
  SET: Option[ProgramPropertiesInput.Create]
)

object CreateProgramInput {
  val Binding: Matcher[CreateProgramInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramPropertiesInput.CreateBinding.Option("SET", rInput)
      ) => rInput.map(apply)
    }
}



