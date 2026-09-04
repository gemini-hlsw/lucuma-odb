// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class RegenerateProposalSummariesInput(programId: Program.Id)

object RegenerateProposalSummariesInput:
  val Binding: Matcher[RegenerateProposalSummariesInput] =
    ObjectFieldsBinding.rmap:
      case List(ProgramIdBinding("programId", rPid)) => rPid.map(apply)
