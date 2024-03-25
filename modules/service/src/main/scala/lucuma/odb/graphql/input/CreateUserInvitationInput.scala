// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.model.Program
import lucuma.odb.data.EmailAddress
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.data.ProgramUserSupportType
import lucuma.odb.data.Tag
import lucuma.odb.graphql.binding.EmailAddressBinding
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
  def recipientEmail: EmailAddress

object CreateUserInvitationInput:

  sealed abstract class Support(val supportType: ProgramUserSupportType) extends CreateUserInvitationInput(ProgramUserRole.Support)

  final case class Coi(programId: Program.Id, recipientEmail: EmailAddress) extends CreateUserInvitationInput(ProgramUserRole.Coi)
  final case class Observer(programId: Program.Id, recipientEmail: EmailAddress) extends CreateUserInvitationInput(ProgramUserRole.Observer)
  final case class StaffSupport(programId: Program.Id, recipientEmail: EmailAddress) extends Support(ProgramUserSupportType.Staff)
  final case class NgoSupportSupport(programId: Program.Id, supportPartner: Tag, recipientEmail: EmailAddress) extends Support(ProgramUserSupportType.Partner)

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        EmailAddressBinding("recipientEmail", rRecipientEmail),
        ProgramUserRoleBinding("role", rRole),
        ProgramUserSupportRoleTypeBinding.Option("supportType", rSupport),
        TagBinding.Option("supportPartner", rPartner)
      ) =>
        (rProgramId, rRecipientEmail, rRole, rSupport, rPartner).parTupled.flatMap:
          case (pid, recip, ProgramUserRole.Coi, None, None)                   => Result(Coi(pid, recip))
          case (pid, recip, ProgramUserRole.Observer, None, None)              => Result(Observer(pid, recip))
          case (pid, recip, ProgramUserRole.Support, Some(Staff), None)        => Result(StaffSupport(pid, recip))
          case (pid, recip, ProgramUserRole.Support, Some(Partner), Some(tag)) => Result(NgoSupportSupport(pid, tag, recip))
          case _                                                        => Matcher.validationFailure("Invalid combination of role, support type, and partner.")