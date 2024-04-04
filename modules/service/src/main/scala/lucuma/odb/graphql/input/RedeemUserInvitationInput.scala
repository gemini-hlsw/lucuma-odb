// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.UserInvitation
import lucuma.odb.graphql.binding.BooleanBinding
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

final case class RedeemUserInvitationInput(key: UserInvitation, accept: Boolean)

object RedeemUserInvitationInput:

  val Binding: Matcher[RedeemUserInvitationInput] =
    ObjectFieldsBinding.rmap {
      case List(
        UserInvitationInput.Binding("key", rKey),
        BooleanBinding("accept", rAccept)
      ) => (rKey, rAccept).mapN(RedeemUserInvitationInput.apply)
    }
