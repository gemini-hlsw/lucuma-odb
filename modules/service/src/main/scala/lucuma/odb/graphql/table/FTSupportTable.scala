// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait FTSupportTable[F[_]] extends BaseMapping[F] {

  object FTSupportTable extends TableDef("t_ft_proposal_support") {
    val ProgramId  = col("c_program_id", program_id)
    val ProgramUserId  = col("c_program_user_id", program_user_id)
  }

}
