// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import lucuma.core.model.ProgramUser
import lucuma.odb.graphql.binding.*

case class UnlinkUserInput(
  programUserId: ProgramUser.Id
)

object UnlinkUserInput:
  val Binding: Matcher[UnlinkUserInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramUserIdBinding("programUserId", rProgramUserId)
      ) => rProgramUserId.map(UnlinkUserInput.apply)