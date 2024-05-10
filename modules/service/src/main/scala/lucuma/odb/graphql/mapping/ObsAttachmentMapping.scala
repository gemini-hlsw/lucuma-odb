// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.predicate.Predicates

import table.ObsAttachmentTable
import table.ProgramTable

trait ObsAttachmentMapping[F[_]]
  extends ObsAttachmentTable[F]
     with ProgramTable[F]
     with Predicates[F] {

  lazy val ObsAttachmentMapping =
    ObjectMapping(ObsAttachmentType)(
      SqlField("id", ObsAttachmentTable.Id, key = true),
      SqlField("program_id", ObsAttachmentTable.ProgramId, hidden = true),
      SqlField("attachmentType", ObsAttachmentTable.AttachmentType),
      SqlField("fileName", ObsAttachmentTable.FileName),
      SqlField("description", ObsAttachmentTable.Description),
      SqlField("checked", ObsAttachmentTable.Checked),
      SqlField("fileSize", ObsAttachmentTable.FileSize),
      SqlField("updatedAt", ObsAttachmentTable.UpdatedAt),
      SqlObject("program", Join(ObsAttachmentTable.ProgramId, ProgramTable.Id)),
    )

}
