// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import lucuma.core.model.UserInvitation
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.StringBinding

object UserInvitationInput:
  val Binding: Matcher[UserInvitation] =
    StringBinding.emap: s =>
      UserInvitation.fromString.getOption(s).toRight(s"Invalid user invitation key: $s")
