// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait AttachmentTable[F[_]] extends BaseMapping[F] {

  object AttachmentTable extends TableDef("t_attachment") {
    val ProgramId      = col("c_program_id", program_id)
    val Id             = col("c_attachment_id", attachment_id)
    val AttachmentType = col("c_attachment_type", attachment_type)
    val FileName       = col("c_file_name", text_nonempty)
    val Description    = col("c_description", text_nonempty.opt)
    val Checked        = col("c_checked", bool)
    val FileSize       = col("c_file_size", int8)
    val UpdatedAt      = col("c_updated_at", core_timestamp)
  }
}
