// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class DeleteProposalInput(
  programId: Program.Id
)

object DeleteProposalInput {
  val Binding: Matcher[DeleteProposalInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid)
      ) => rPid.map(apply)
    }
}
