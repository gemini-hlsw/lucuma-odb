// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.all.*
import lucuma.core.model.ProgramUser
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding
import lucuma.odb.graphql.binding.ProgramUserIdBinding

final case class RevokeUserInvitationInput(id: ProgramUser.Id)

object RevokeUserInvitationInput:

  val Binding: Matcher[RevokeUserInvitationInput] =
    ObjectFieldsBinding.rmap:
      case List(ProgramUserIdBinding("id", rId)) => rId.map(RevokeUserInvitationInput.apply)