// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.AttachmentTable
import lucuma.odb.graphql.util.MappingExtras

import table.AttachmentTable
import table.ProgramTable

trait AttachmentMapping[F[_]] extends AttachmentTable[F] with ProgramTable[F] {
  
  lazy val AttachmentMapping =
    ObjectMapping(
      tpe = AttachmentType,
      fieldMappings = List(
        SqlField("id", AttachmentTable.Id, key = true),
        SqlField("program_id", AttachmentTable.ProgramId, hidden = true),
        SqlField("attachmentType", AttachmentTable.AttachmentType),
        SqlField("fileName", AttachmentTable.FileName),
        SqlField("description", AttachmentTable.Description),
        SqlField("checked", AttachmentTable.Checked),
        SqlField("fileSize", AttachmentTable.FileSize),
        SqlField("updatedAt", AttachmentTable.UpdatedAt),
        SqlObject("program", Join(AttachmentTable.ProgramId, ProgramTable.Id))
      )
    )
}
