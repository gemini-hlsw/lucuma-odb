// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.*

trait EmailMapping[F[_]]
  extends ProgramTable[F]
  with EmailTable[F] {

  lazy val EmailMapping = 
    ObjectMapping(EmailType)(
      SqlField("id", EmailTable.EmailId, key = true, hidden = true),
      SqlField("senderEmail", EmailTable.SenderEmail),
      SqlField("recipientEmail", EmailTable.RecipientEmail),
      SqlField("subject", EmailTable.Subject),
      SqlField("textMessage", EmailTable.TextMessage),
      SqlField("htmlMessage", EmailTable.HtmlMessage),
      SqlField("originalTime", EmailTable.OriginalTime),
      SqlField("status", EmailTable.Status),
      SqlField("statusTime", EmailTable.StatusTime)
    )
  
}
