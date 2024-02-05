// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._

trait UserInvitationTable[F[_]] extends BaseMapping[F]:
 object UserInvitationTable extends TableDef("t_invitation"):
    val InvitationId   = col("c_invitation_id", user_invitation_id)
    val Status         = col("c_status", user_invitation_status)
    val IssuerId       = col("c_issuer_id", user_id)
    val ProgramId      = col("c_program_id", program_id)
    val Role           = col("c_role", program_user_role)
    val SupportType    = col("c_support_type", program_user_support_type.opt)
    val SupportPartner = col("c_support_partner", tag.opt)
    val RedeemerId     = col("c_redeemer_id", user_id)
