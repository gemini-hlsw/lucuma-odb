// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.UserInvitationTable

trait RedeemUserInvitationResultMapping[F[_]] extends UserInvitationTable[F]:

  lazy val RedeemUserInvitationResultMapping =
    ObjectMapping(RedeemUserInvitationResultType)(
      SqlField("id", UserInvitationTable.InvitationId, key = true, hidden = true),
      SqlObject("invitation")
    )