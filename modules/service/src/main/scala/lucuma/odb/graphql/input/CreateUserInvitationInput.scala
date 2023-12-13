// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.Program
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ProgramIdBinding
import lucuma.odb.graphql.binding.ProgramUserRoleBinding
import lucuma.odb.graphql.binding.ProgramUserSupportRoleTypeBinding
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.TagBinding
import grackle.Result
import ProgramUserRole.*
import ProgramUserSupportType.*

final case class CreateUserInvitationInput(
  programId: Program.Id,
  role: ProgramUserRole,
  supportType: Option[ProgramUserSupportType],
  supportPartner: Option[Tag]
)

object CreateUserInvitationInput:

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        ProgramUserRoleBinding("role", rRole),
        ProgramUserSupportRoleTypeBinding.Option("supportType", rSupport),
        TagBinding.Option("supportPartner", rPartner)
      ) =>
        (rProgramId, rRole, rSupport, rPartner).parTupled.flatMap:
          case (pid, role@(Coi | Observer), None, None) => Result(CreateUserInvitationInput(pid, role, None, None))
          case (pid, Support, Some(Staff), None)        => Result(CreateUserInvitationInput(pid, Support, Some(Staff), None))
          case (pid, Support, Some(Partner), Some(tag)) => Result(CreateUserInvitationInput(pid, Support, Some(Partner), Some(tag)))
          case _                                        => Result.failure("Invalid combination of role, support type, and partner.")