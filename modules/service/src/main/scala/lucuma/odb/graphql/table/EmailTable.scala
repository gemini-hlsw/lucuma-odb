// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait EmailTable[F[_]] extends BaseMapping[F]:
  object EmailTable extends TableDef("t_email"):
    val EmailId        = col("c_email_id", email_id)
    val ProgramId      = col("c_program_id", program_id)
    val SenderEmail    = col("c_sender_email", email_address)
    val RecipientEmail = col("c_recipient_email", email_address)
    val Subject        = col("c_subject", text_nonempty)
    val TextMessage    = col("c_text_message", text_nonempty)
    val HtmlMessage    = col("c_html_message", text_nonempty.opt)
    val OriginalTime   = col("c_original_time", core_timestamp)
    val Status         = col("c_status", email_status)
    val StatusTime     = col("c_status_time", core_timestamp)
