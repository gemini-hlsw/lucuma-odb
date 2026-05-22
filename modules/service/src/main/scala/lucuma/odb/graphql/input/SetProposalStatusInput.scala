// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.enums.ProposalStatus
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.odb.graphql.binding.*

final case class SetProposalStatusInput(
  programId: Option[Program.Id],
  proposalReference: Option[ProposalReference],
  programReference:  Option[ProgramReference],
  status:            ProposalStatus
)

object SetProposalStatusInput {
  val Binding: Matcher[SetProposalStatusInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding.Option("programId", rPid),
        ProposalReferenceBinding.Option("proposalReference", rProp),
        ProgramReferenceBinding.Option("programReference", rProg),
        ProposalStatusBinding("status", rPs)
      ) => (rPid, rProp, rProg, rPs).parMapN(apply)
    }
}
