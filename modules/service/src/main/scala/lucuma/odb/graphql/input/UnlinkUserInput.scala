// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.binding.*

case class UnlinkUserInput(
  programId: Program.Id,
  userId: User.Id
)

object UnlinkUserInput:

  val Binding: Matcher[UnlinkUserInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        UserIdBinding("userId", rUserId),
      ) =>
        (rProgramId, rUserId).parTupled.map(UnlinkUserInput.apply)

