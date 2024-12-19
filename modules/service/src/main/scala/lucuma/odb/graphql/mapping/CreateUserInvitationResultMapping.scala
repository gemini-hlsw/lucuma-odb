// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.core.model.UserInvitation

import table.UserInvitationTable

trait CreateUserInvitationResultMapping[F[_]] extends UserInvitationTable[F]:

  lazy val CreateUserInvitationResultMapping =
    ObjectMapping(CreateUserInvitationResultType)(
      SqlField("id", UserInvitationTable.InvitationId, key = true, hidden = true),
      SqlObject("invitation"),
      CursorField("key", c => c.envR[UserInvitation]("inv").map(UserInvitation.fromString.reverseGet), List("id"))
    )