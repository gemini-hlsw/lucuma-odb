// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*

trait ExecutionTimestampsView[F[_]] extends BaseMapping[F]:

  object ExecutionTimestampsView extends TableDef("v_execution_timestamps"):
    val ObservationId  = col("c_observation_id",   observation_id)
    val FirstEventTime = col("c_first_event_time", core_timestamp.opt)
    val LastEventTime  = col("c_last_event_time",  core_timestamp.opt)
    val CompletionTime = col("c_completion_time",  core_timestamp.opt)
