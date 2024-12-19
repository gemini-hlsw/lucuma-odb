// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import grackle.Result
import lucuma.core.data.EmailAddress
import lucuma.core.model.ProgramUser
import lucuma.odb.graphql.binding.EmailAddressBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ProgramUserIdBinding

case class CreateUserInvitationInput(
  programUserId:  ProgramUser.Id,
  recipientEmail: EmailAddress
)

object CreateUserInvitationInput:

  val Binding: Matcher[CreateUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ProgramUserIdBinding("programUserId", rProgramUserId),
        EmailAddressBinding("recipientEmail", rRecipientEmail)
      ) =>
        (rProgramUserId, rRecipientEmail).parMapN(CreateUserInvitationInput.apply)