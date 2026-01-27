// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.*

trait AtomRecordView[F[_]] extends BaseMapping[F]:
  object AtomRecordView extends TableDef("v_atom_record"):
    val Id: ColumnRef             = col("c_atom_id",          atom_id)
    val Instrument: ColumnRef     = col("c_instrument",       instrument)
    val ObservationId: ColumnRef  = col("c_observation_id",   observation_id)
    val SequenceType: ColumnRef   = col("c_sequence_type",    sequence_type)
    val Description: ColumnRef    = col("c_description",      text_nonempty.opt)
    val ExecutionState: ColumnRef = col("c_execution_state",  atom_execution_state)

    // The t_atom table from which the view is derived has optional first and
    // last event times.  The view, though, filters out any rows without
    // timestamps.
    val ExecutionOrder: ColumnRef = col("c_execution_order",  int4_pos)
    val FirstEventTime: ColumnRef = col("c_first_event_time", core_timestamp)
    val LastEventTime:  ColumnRef = col("c_last_event_time",  core_timestamp)