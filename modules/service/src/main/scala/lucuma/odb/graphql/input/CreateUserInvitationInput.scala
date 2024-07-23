// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.data.EmailAddress
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.asFailure
import lucuma.odb.data.ProgramUserRole as PUR
import lucuma.odb.graphql.binding.EmailAddressBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.PartnerBinding
import lucuma.odb.graphql.binding.ProgramIdBinding
import lucuma.odb.graphql.binding.ProgramUserRoleBinding

enum CreateUserInvitationInput:
  def programId: Program.Id
  def recipientEmail: EmailAddress
  case Coi(programId: Program.Id, recipientEmail: EmailAddress, partner: Option[Partner])
  case CoiRO(programId: Program.Id, recipientEmail: EmailAddress, partner: Option[Partner])
  case Support(programId: Program.Id, recipientEmail: EmailAddress)

object CreateUserInvitationInput:

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        EmailAddressBinding("recipientEmail", rRecipientEmail),
        ProgramUserRoleBinding("role", rRole),
        PartnerBinding.Option("partner", rPartner),
      ) =>
        (rProgramId, rRecipientEmail, rRole, rPartner).parTupled.flatMap:
          case (pid, email, PUR.Coi, p)        => Result(CreateUserInvitationInput.Coi(pid, email, p))
          case (pid, email, PUR.CoiRO, p)      => Result(CreateUserInvitationInput.CoiRO(pid, email, p))
          case (pid, email, PUR.Support, None) => Result(CreateUserInvitationInput.Support(pid, email))
          case (_, _, PUR.Support, Some(_))    => OdbError.InvalidArgument("A partner may not be specified for support invitations.".some).asFailure
