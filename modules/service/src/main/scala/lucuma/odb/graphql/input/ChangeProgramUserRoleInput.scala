// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.ProgramUser
import lucuma.odb.graphql.binding.*

case class ChangeProgramUserRoleInput(
  programUserId: ProgramUser.Id,
  newRole:       ProgramUserRole
)

object ChangeProgramUserRoleInput:
  def ensuringNotPi(role: ProgramUserRole): Result[ProgramUserRole] =
    role match
      case ProgramUserRole.Pi => Result.failure(s"PIs are added at program creation time.")
      case _                  => role.success

  val Binding: Matcher[ChangeProgramUserRoleInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramUserIdBinding("programUserId", rProgramUserId),
        ProgramUserRoleBinding("newRole", rNewRole)
      ) => (
        rProgramUserId,
        rNewRole.flatMap(ensuringNotPi)
      ).parMapN(ChangeProgramUserRoleInput.apply)