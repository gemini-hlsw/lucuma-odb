// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.data.EmailAddress
import lucuma.core.enums.ProgramUserRole as PUR
import lucuma.core.model.PartnerLink
import lucuma.core.model.PartnerLink.HasUnspecifiedPartner
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.graphql.binding.EmailAddressBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ProgramIdBinding
import lucuma.odb.graphql.binding.ProgramUserRoleBinding

enum CreateUserInvitationInput:
  def programId: Program.Id
  def recipientEmail: EmailAddress
  case Coi(programId: Program.Id, recipientEmail: EmailAddress, partnerLink: PartnerLink)
  case CoiRO(programId: Program.Id, recipientEmail: EmailAddress, partnerLink: PartnerLink)
  case Support(programId: Program.Id, recipientEmail: EmailAddress, tpe: PUR.SupportPrimary.type | PUR.SupportSecondary.type)

object CreateUserInvitationInput:

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        EmailAddressBinding("recipientEmail", rRecipientEmail),
        ProgramUserRoleBinding("role", rRole),
        PartnerLinkInput.Binding.Option("partnerLink", rPartnerLink)
      ) =>
        (rProgramId, rRecipientEmail, rRole, rPartnerLink).parTupled.flatMap:
          case (pid, email, PUR.Coi, p)        => Result(CreateUserInvitationInput.Coi(pid, email, p.getOrElse(HasUnspecifiedPartner)))
          case (pid, email, PUR.CoiRO, p)      => Result(CreateUserInvitationInput.CoiRO(pid, email, p.getOrElse(HasUnspecifiedPartner)))
          case (pid, email, t @ (PUR.SupportPrimary | PUR.SupportSecondary), None) => Result(CreateUserInvitationInput.Support(pid, email, t))
          case (_, _, PUR.SupportPrimary | PUR.SupportSecondary, Some(_))    => OdbError.InvalidArgument("A partner may not be specified for support invitations.".some).asFailure
          case (_, _, PUR.Pi, _)               => OdbError.InvalidArgument("Cannot create an invitation for the PI.".some).asFailure
