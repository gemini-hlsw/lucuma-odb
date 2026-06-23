// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Program
import lucuma.odb.graphql.binding.*

case class SetProgramResourceLimitInput(
  programId: Program.Id,
  limit:     NonNegInt
)

object SetProgramResourceLimitInput {
  val Binding: Matcher[SetProgramResourceLimitInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramIdBinding("programId", rPid),
        NonNegIntBinding("limit", rLimit)
      ) => (rPid, rLimit).parMapN(apply)
    }
}
