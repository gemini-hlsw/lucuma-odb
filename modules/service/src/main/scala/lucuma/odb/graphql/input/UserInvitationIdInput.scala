// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.UserInvitation
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.StringBinding

object UserInvitationIdInput:
  val Binding: Matcher[UserInvitation.Id] =
    StringBinding.emap: s =>
      UserInvitation.Id.fromString.getOption(s).toRight(s"Invalid user invitation id: $s")
