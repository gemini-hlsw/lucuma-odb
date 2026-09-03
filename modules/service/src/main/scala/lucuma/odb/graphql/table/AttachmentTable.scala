// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.circe.codec.all.*
import skunk.codec.all.*

trait AttachmentTable[F[_]] extends BaseMapping[F]:

  object AttachmentTable extends TableDef("t_attachment") {
    val ProgramId      = col("c_program_id", program_id)
    val Id             = col("c_attachment_id", attachment_id)
    val AttachmentType = col("c_attachment_type", attachment_type)
    val FileName       = col("c_file_name", text_nonempty)
    val MaskName       = col("c_mask_name", text_nonempty.opt)
    val MaskDefinition = col("c_mask_definition", jsonb.opt)
    val Description    = col("c_description", text_nonempty.opt)
    val Checked        = col("c_checked", bool)
    val FileSize       = col("c_file_size", int8)
    val UpdatedAt      = col("c_updated_at", core_timestamp)
    val Partner        = col("c_partner", partner.opt)
    val SummaryStyle   = col("c_summary_style", summary_style.opt)
    val SummaryStyleNN = col("c_summary_style", summary_style)
  }
