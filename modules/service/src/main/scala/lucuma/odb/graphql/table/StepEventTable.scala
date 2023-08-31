// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.execution_event_id
import lucuma.odb.util.Codecs.sequence_type
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Codecs.step_stage


trait StepEventTable[F[_]] extends BaseMapping[F] {

  object StepEventTable extends TableDef("t_step_event") {
    val Id: ColumnRef           = col("c_execution_event_id", execution_event_id)
    val StepId: ColumnRef       = col("c_step_id",            step_id)
    val SequenceType: ColumnRef = col("c_sequence_type",      sequence_type)
    val StepStage: ColumnRef    = col("c_step_stage",         step_stage)
    val Received: ColumnRef     = col("c_received",           core_timestamp)
  }

}