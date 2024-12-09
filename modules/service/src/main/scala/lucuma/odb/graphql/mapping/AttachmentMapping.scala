// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.predicate.Predicates

import table.AttachmentTable
import table.ProgramTable

trait AttachmentMapping[F[_]]
  extends AttachmentTable[F]
     with ProgramTable[F]
     with Predicates[F] {

  lazy val AttachmentMapping =
    ObjectMapping(AttachmentType)(
      SqlField("id", AttachmentTable.Id, key = true),
      SqlField("program_id", AttachmentTable.ProgramId, hidden = true),
      SqlField("attachmentType", AttachmentTable.AttachmentType),
      SqlField("fileName", AttachmentTable.FileName),
      SqlField("description", AttachmentTable.Description),
      SqlField("checked", AttachmentTable.Checked),
      SqlField("fileSize", AttachmentTable.FileSize),
      SqlField("updatedAt", AttachmentTable.UpdatedAt),
      SqlObject("program", Join(AttachmentTable.ProgramId, ProgramTable.Id)),
    )

}
