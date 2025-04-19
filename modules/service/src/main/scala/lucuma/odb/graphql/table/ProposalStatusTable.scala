// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait ProposalStatusTable[F[_]] extends BaseMapping[F] {

  object ProposalStatusTable extends TableDef("t_proposal_status") {
    val Tag     = col("c_tag", tag)
    val Name    = col("c_name", varchar)
    val Ordinal = col("c_ordinal", int2)
  }
}
