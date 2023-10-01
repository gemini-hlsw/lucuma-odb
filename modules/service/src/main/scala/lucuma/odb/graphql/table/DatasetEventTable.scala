// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.dataset_stage
import lucuma.odb.util.Codecs.execution_event_id
import lucuma.odb.util.Codecs.int2_pos
import lucuma.odb.util.Codecs.step_id

trait DatasetEventTable[F[_]] extends BaseMapping[F] {

  object DatasetEventTable extends TableDef("t_dataset_event") {
    val Id: ColumnRef           = col("c_execution_event_id", execution_event_id)

    object DatasetId {
      val StepId: ColumnRef     = col("c_step_id",            step_id)
      val Index: ColumnRef      = col("c_index",              int2_pos)
    }

    val Received: ColumnRef     = col("c_received",           core_timestamp)
    val DatasetStage: ColumnRef = col("c_dataset_stage",      dataset_stage)

  }

}
