// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.odb.graphql.binding.*
import lucuma.core.model.Group
import lucuma.core.model.Observation

case class CreateGroupInput(
  programId:         Option[Program.Id],
  proposalReference: Option[ProposalReference],
  programReference:  Option[ProgramReference],
  SET:               GroupPropertiesInput.Create,
  initialContents:   List[Either[Group.Id, Observation.Id]],
)

object CreateGroupInput {
  val Binding: Matcher[CreateGroupInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        GroupPropertiesInput.CreateBinding.Option("SET", rInput),
        GroupElementInput.Binding.List.Option("initialContents", rInitialContents),
      ) => (rPid, rProp, rProg, rInput, rInitialContents).mapN { (pid, prop, prog, oset, initialContents) =>
        CreateGroupInput(pid, prop, prog, oset.getOrElse(GroupPropertiesInput.Empty), initialContents.foldMap(_.map(_.value)))
      }
    }
}



