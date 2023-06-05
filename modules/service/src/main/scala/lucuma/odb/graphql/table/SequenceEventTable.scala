// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.execution_event_id
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.sequence_command
import lucuma.odb.util.Codecs.visit_id

trait SequenceEventTable[F[_]] extends BaseMapping[F] {

  object SequenceEventTable extends TableDef("t_sequence_event") {
    val Id: ColumnRef              = col("c_execution_event_id", execution_event_id)
    val VisitId: ColumnRef         = col("c_visit_id",           visit_id)
    val Received: ColumnRef        = col("c_received",           core_timestamp)
    val SequenceCommand: ColumnRef = col("c_sequence_command",   sequence_command)
  }

}