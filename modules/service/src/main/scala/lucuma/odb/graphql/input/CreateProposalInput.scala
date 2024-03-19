// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.odb.graphql.binding.*

case class CreateProposalInput(
  programId: Option[Program.Id],
  proposalReference: Option[ProposalReference],
  programReference: Option[ProgramReference],
  SET: ProposalPropertiesInput.Create
)

object CreateProposalInput {
  val Binding: Matcher[CreateProposalInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        ProposalPropertiesInput.CreateBinding("SET", rSet)
      ) => (rPid, rProp, rProg, rSet).parMapN(apply)
    }
}
