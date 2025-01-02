// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait UserInvitationTable[F[_]] extends BaseMapping[F]:
 object UserInvitationTable extends TableDef("t_invitation"):
    val InvitationId   = col("c_invitation_id", user_invitation_id)
    val ProgramUserId  = col("c_program_user_id", program_user_id)
    val Status         = col("c_status", user_invitation_status)
    val IssuerId       = col("c_issuer_id", user_id)
    val ProgramId      = col("c_program_id", program_id)
    val RecipientEmail = col("c_recipient_email", email_address)
    val EmailId        = col("c_email_id", email_id)
