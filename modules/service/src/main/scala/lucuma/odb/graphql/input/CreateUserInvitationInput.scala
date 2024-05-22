// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.data.EmailAddress
import lucuma.core.model.Program
import lucuma.odb.data.ProgramUserRole
import lucuma.odb.graphql.binding.EmailAddressBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ProgramIdBinding
import lucuma.odb.graphql.binding.ProgramUserRoleBinding

case class CreateUserInvitationInput(programId: Program.Id, recipientEmail: EmailAddress, role: ProgramUserRole)

object CreateUserInvitationInput:

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramIdBinding("programId", rProgramId),
        EmailAddressBinding("recipientEmail", rRecipientEmail),
        ProgramUserRoleBinding("role", rRole),
      ) =>
        (rProgramId, rRecipientEmail, rRole).parMapN(apply)
