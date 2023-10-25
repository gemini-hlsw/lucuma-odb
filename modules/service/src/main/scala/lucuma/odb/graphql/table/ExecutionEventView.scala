// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.dataset_id
import lucuma.odb.util.Codecs.dataset_stage
import lucuma.odb.util.Codecs.execution_event_id
import lucuma.odb.util.Codecs.execution_event_type
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.sequence_command
import lucuma.odb.util.Codecs.step_id
import lucuma.odb.util.Codecs.step_stage
import lucuma.odb.util.Codecs.visit_id

trait ExecutionEventView[F[_]] extends BaseMapping[F] {

  object ExecutionEventView extends TableDef("v_execution_event") {
    val Id: ColumnRef              = col("c_execution_event_id", execution_event_id)
    val EventType: ColumnRef       = col("c_event_type",         execution_event_type)
    val Received: ColumnRef        = col("c_received",           core_timestamp)

    val ObservationId: ColumnRef   = col("c_observation_id",     observation_id)
    val VisitId: ColumnRef         = col("c_visit_id",           visit_id)
    val StepId: ColumnRef          = col("c_step_id",            step_id)
    val DatasetId: ColumnRef       = col("c_dataset_id",         dataset_id)

    val SequenceCommand: ColumnRef = col("c_sequence_command",   sequence_command)
    val StepStage: ColumnRef       = col("c_step_stage",         step_stage)
    val DatasetStage: ColumnRef    = col("c_dataset_stage",      dataset_stage)
  }

}
