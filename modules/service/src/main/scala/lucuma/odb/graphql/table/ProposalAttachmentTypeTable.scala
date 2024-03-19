// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait ProposalAttachmentTypeTable[F[_]] extends BaseMapping[F] {

  object ProposalAttachmentTypeTable extends TableDef("t_proposal_attachment_type") {
    val Tag       = col("c_tag", tag)
    val ShortName = col("c_short_name", varchar)
    val LongName  = col("c_long_name", varchar)
  }
}
