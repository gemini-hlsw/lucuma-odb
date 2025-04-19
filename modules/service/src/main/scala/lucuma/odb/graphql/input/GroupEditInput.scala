// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.apply.*
import lucuma.core.model.Group
import lucuma.core.model.Program
import lucuma.odb.data.Nullable
import lucuma.odb.graphql.binding.*

final case class GroupEditInput(
  groupId:   Nullable[Group.Id],
  programId: Option[Program.Id]
)

object GroupEditInput {

  val Binding = ObjectFieldsBinding.rmap {
    case List(
      GroupIdBinding.Nullable("groupId", rGroupId),
      ProgramIdBinding.Option("programId", rProgramId)
    ) =>
      (rGroupId, rProgramId).mapN(apply)
  }

}