// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import table.ProgramTable
import table.ProposalAttachmentTable

trait ProposalAttachmentMapping[F[_]] extends ProposalAttachmentTable[F] with ProgramTable[F] {

  lazy val ProposalAttachmentMapping =
    ObjectMapping(TypeMatch(ProposalAttachmentType))(
      SqlField("program_id", ProposalAttachmentTable.ProgramId, key = true, hidden = true),
      SqlField("attachmentType", ProposalAttachmentTable.AttachmentType, key = true),
      SqlField("fileName", ProposalAttachmentTable.FileName),
      SqlField("fileSize", ProposalAttachmentTable.FileSize),
      SqlField("updatedAt", ProposalAttachmentTable.UpdatedAt),
      SqlObject("program", Join(ProposalAttachmentTable.ProgramId, ProgramTable.Id))
    )

}
