// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.UserInvitation
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

final case class RevokeUserInvitationInput(id: UserInvitation.Id)

object RevokeUserInvitationInput:

  val Binding: Matcher[RevokeUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(
        UserInvitationIdInput.Binding("id", rId),
      ) => rId.map(RevokeUserInvitationInput.apply)