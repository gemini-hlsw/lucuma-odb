// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.numeric.*

trait TimingWindowView[F[_]] extends BaseMapping[F] {

  object TimingWindowView extends TableDef("v_timing_window") {
    val Id: ColumnRef              = col("c_timing_window_id", int8)
    val ObservationId: ColumnRef   = col("c_observation_id", observation_id)
    val Inclusion: ColumnRef       = col("c_inclusion", timing_window_inclusion)
    val Start: ColumnRef           = col("c_start", core_timestamp)
    object End {
      val SyntheticId: ColumnRef   = col("c_end_id", int8)
      val Type: ColumnRef          = col("c_end_type", timing_window_end_type)
      val At: ColumnRef            = col("c_end_at", core_timestamp)
      val After: ColumnRef         = col("c_end_after", time_span)
      object Repeat {
        val SyntheticId: ColumnRef = col("c_repeat_id", int8)
        val Period: ColumnRef      = col("c_repeat_period", time_span)
        val Times: ColumnRef       = col("c_repeat_times", int4.opt)
      }
    }
  }

}
