// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import table.ObsAttachmentFileExtTable

trait ObsAttachmentFileExtMapping[F[_]] extends ObsAttachmentFileExtTable[F] {

  lazy val ObsAttachmentFileExtMapping =
    ObjectMapping(ObsAttachmentFileExtType)(
      SqlField("attachmentType", ObsAttachmentFileExtTable.AttachmentType, key = true, hidden = true),
      SqlField("fileExtension", ObsAttachmentFileExtTable.FileExtension, key = true)
    )

}
