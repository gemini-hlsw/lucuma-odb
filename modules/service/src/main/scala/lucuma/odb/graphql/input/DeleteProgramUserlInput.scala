// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.ProgramUser
import lucuma.odb.graphql.binding.*

case class DeleteProgramUserInput(
  programUserId: ProgramUser.Id
)

object DeleteProgramUserInput:
  val Binding: Matcher[DeleteProgramUserInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramUserIdBinding("programUserId", rRid)
      ) => rRid.map(apply)