// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait SummaryGenerationView[F[_]] extends BaseMapping[F]:

  // Backed by v_summary_generation (V1314), which aggregates t_summary_job to
  // one row per program.
  object SummaryGenerationView extends TableDef("v_summary_generation"):
    val ProgramId   = col("c_program_id", program_id)
    val State       = col("c_state", summary_generation_state)
    val RequestedAt = col("c_requested_at", core_timestamp.opt)
