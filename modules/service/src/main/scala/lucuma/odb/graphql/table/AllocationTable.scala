// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait AllocationTable[F[_]] extends BaseMapping[F] {

  object AllocationTable extends TableDef("t_allocation") {
    val ProgramId   = col("c_program_id",   program_id)
    val Category    = col("c_ta_category",  time_accounting_category)
    val ScienceBand = col("c_science_band", science_band)
    val Duration    = col("c_duration",     time_span)
  }

}
