// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.ObsAttachmentFileExtTable
import lucuma.odb.graphql.table.ObsAttachmentTypeTable

trait ObsAttachmentTypeMetaMapping[F[_]] extends ObsAttachmentTypeTable[F] with ObsAttachmentFileExtTable[F] {

  lazy val ObsAttachmentTypeMetaMapping =
    ObjectMapping(ObsAttachmentTypeMetaType)(
      SqlField("tag", ObsAttachmentTypeTable.Tag, key = true),
      SqlField("shortName", ObsAttachmentTypeTable.ShortName),
      SqlField("longName", ObsAttachmentTypeTable.LongName),
      SqlObject("fileExtensions", Join(ObsAttachmentTypeTable.Tag, ObsAttachmentFileExtTable.AttachmentType))
    )

}
