// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class CreateProposalInput(
  programId: Program.Id,
  SET: ProposalPropertiesInput.Create
)

object CreateProposalInput {
  val Binding: Matcher[CreateProposalInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid),
        ProposalPropertiesInput.CreateBinding("SET", rSet)
      ) => (rPid, rSet).parMapN(apply)
    }
}
