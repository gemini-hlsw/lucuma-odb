// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.*

trait UserInvitationMapping[F[_]] extends ProgramTable[F] with UserTable[F] with UserInvitationTable[F] {

  lazy val UserInvitationMapping =
    ObjectMapping(
      tpe = UserInvitationType,
      fieldMappings = List(
        SqlField("id", UserInvitationTable.InvitationId, key = true),
        SqlField("status", UserInvitationTable.Status),
        SqlObject("issuer", Join(UserInvitationTable.IssuerId, UserTable.UserId)),
        SqlObject("program", Join(UserInvitationTable.ProgramId, ProgramTable.Id)),
        SqlField("recipientEmail", UserInvitationTable.RecipientEmail),
        SqlField("role", UserInvitationTable.Role),
        SqlField("supportType", UserInvitationTable.SupportType),
        SqlField("supportPartner", UserInvitationTable.SupportPartner),
        SqlObject("redeemer", Join(UserInvitationTable.RedeemerId, UserTable.UserId)),
      )
    )

  }

