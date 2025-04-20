// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.odb.graphql.binding.*

case class UpdateProposalInput(
  programId: Option[Program.Id],
  proposalReference: Option[ProposalReference],
  programReference: Option[ProgramReference],
  SET: ProposalPropertiesInput.Edit
)

object UpdateProposalInput {
  val Binding: Matcher[UpdateProposalInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        ProposalPropertiesInput.EditBinding("SET", rSet)
      ) => (rPid, rProp, rProg, rSet).parMapN(apply)
    }
}
