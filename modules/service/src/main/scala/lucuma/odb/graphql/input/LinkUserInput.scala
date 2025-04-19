// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.ProgramUser
import lucuma.core.model.User
import lucuma.odb.graphql.binding.*

case class LinkUserInput(
  programUserId: ProgramUser.Id,
  userId:        User.Id
)

object LinkUserInput:
  val Binding: Matcher[LinkUserInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramUserIdBinding("programUserId", rProgramUserId),
        UserIdBinding("userId", rUserId)
      ) =>
        (rProgramUserId, rUserId).parTupled.map(LinkUserInput.apply)
