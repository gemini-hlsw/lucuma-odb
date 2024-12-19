// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping

import table.*

trait UserInvitationMapping[F[_]]
  extends ProgramTable[F]
  with ProgramUserTable[F]
  with UserTable[F]
  with UserInvitationTable[F]
  with EmailTable[F]:

  lazy val UserInvitationMapping =
    ObjectMapping(UserInvitationType)(
      SqlField("id", UserInvitationTable.InvitationId, key = true),
      SqlField("status", UserInvitationTable.Status),
      SqlObject("issuer", Join(UserInvitationTable.IssuerId, UserTable.UserId)),
      SqlField("recipientEmail", UserInvitationTable.RecipientEmail),
      SqlObject("programUser", Join(UserInvitationTable.ProgramUserId, ProgramUserTable.ProgramUserId)),
      SqlObject("email", Join(UserInvitationTable.EmailId, EmailTable.EmailId))
    )