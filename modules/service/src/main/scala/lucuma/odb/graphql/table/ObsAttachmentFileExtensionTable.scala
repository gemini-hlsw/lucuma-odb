// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait ObsAttachmentFileExtTable[F[_]] extends BaseMapping[F]{

  object ObsAttachmentFileExtTable extends TableDef("t_obs_attachment_file_ext") {
    val AttachmentType = col("c_attachment_type", tag)
    val FileExtension  = col("c_file_extension", text_nonempty)
  }
}
