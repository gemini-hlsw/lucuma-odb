// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.UserInvitationTable
import lucuma.odb.data.UserInvitation

trait CreateUserInvitationResultMapping[F[_]] extends UserInvitationTable[F] {

  lazy val CreateUserInvitationResultMapping =
    ObjectMapping(
      tpe = CreateUserInvitationResultType,
      fieldMappings = List(
        SqlField("id", UserInvitationTable.InvitationId, key = true),
        SqlObject("invitation"),
        CursorField("key", c => c.envR[UserInvitation]("inv").map(UserInvitation.fromString.reverseGet), List("id"))
      ),
    )

  }

