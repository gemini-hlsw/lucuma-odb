// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.graphql.binding.*

final case class TargetEditInput(
  targetId: Option[Target.Id],
  programId:     Option[Program.Id]
)

object TargetEditInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      TargetIdBinding.Option("targetId", rTargetId),
      ProgramIdBinding.Option("programId", rProgramId)
    ) =>
      (rTargetId, rProgramId).mapN(apply)
  }

}