// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.odb.graphql.binding._

case class CreateGroupInput(
  programId: Program.Id,
  SET: GroupPropertiesInput.Create
)

object CreateGroupInput {
  val Binding: Matcher[CreateGroupInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rProgramId),
        GroupPropertiesInput.CreateBinding.Option("SET", rInput)
      ) => (rProgramId, rInput).mapN { (pid, oset) =>
        CreateGroupInput(pid, oset.getOrElse(GroupPropertiesInput.Empty))
      }
    }
}



