// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Program
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ProgramIdBinding
import lucuma.odb.graphql.binding.ProgramUserRoleBinding
import lucuma.odb.graphql.binding.ProgramUserSupportRoleTypeBinding
import lucuma.odb.graphql.binding.TagBinding

import ProgramUserRole.*
import ProgramUserSupportType.*

sealed abstract class CreateUserInvitationInput(val role: ProgramUserRole):
  def programId: Program.Id

object CreateUserInvitationInput:

  sealed abstract class Support(val supportType: ProgramUserSupportType) extends CreateUserInvitationInput(ProgramUserRole.Support)

  final case class Coi(programId: Program.Id) extends CreateUserInvitationInput(ProgramUserRole.Coi)
  final case class Observer(programId: Program.Id) extends CreateUserInvitationInput(ProgramUserRole.Observer)
  final case class StaffSupport(programId: Program.Id) extends Support(ProgramUserSupportType.Staff)
  final case class NgoSupportSupport(programId: Program.Id, supportPartner: Tag) extends Support(ProgramUserSupportType.Partner)

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        ProgramUserRoleBinding("role", rRole),
        ProgramUserSupportRoleTypeBinding.Option("supportType", rSupport),
        TagBinding.Option("supportPartner", rPartner)
      ) =>
        (rProgramId, rRole, rSupport, rPartner).parTupled.flatMap:
          case (pid, ProgramUserRole.Coi, None, None)                   => Result(Coi(pid))
          case (pid, ProgramUserRole.Observer, None, None)              => Result(Observer(pid))
          case (pid, ProgramUserRole.Support, Some(Staff), None)        => Result(StaffSupport(pid))
          case (pid, ProgramUserRole.Support, Some(Partner), Some(tag)) => Result(NgoSupportSupport(pid, tag))
          case _                                                        => Matcher.validationFailure("Invalid combination of role, support type, and partner.")