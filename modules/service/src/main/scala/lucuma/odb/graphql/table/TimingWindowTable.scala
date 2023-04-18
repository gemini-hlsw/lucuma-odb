// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._

trait TimingWindowTable[F[_]] extends BaseMapping[F] {

  object TimingWindowTable extends TableDef("t_timing_window") {
    val TimingWindowId: ColumnRef = col("c_timing_window_id", timing_window_id)
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val Inclusion: ColumnRef = col("c_inclusion", timing_window_inclusion)
    val Start: ColumnRef = col("c_start", data_timestamp)

  }

}
