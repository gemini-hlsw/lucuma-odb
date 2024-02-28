// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.odb.data.ProgramReference
import lucuma.odb.graphql.binding._

case class CreateGroupInput(
  programId:        Option[Program.Id],
  programReference: Option[ProgramReference],
  SET:              GroupPropertiesInput.Create
)

object CreateGroupInput {
  val Binding: Matcher[CreateGroupInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProgramReferenceBinding.Option("programReference", rRef),
        GroupPropertiesInput.CreateBinding.Option("SET", rInput)
      ) => (rPid, rRef, rInput).mapN { (pid, ref, oset) =>
        CreateGroupInput(pid, ref, oset.getOrElse(GroupPropertiesInput.Empty))
      }
    }
}



