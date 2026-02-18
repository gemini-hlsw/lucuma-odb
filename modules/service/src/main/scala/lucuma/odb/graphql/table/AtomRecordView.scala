// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.*

trait AtomRecordView[F[_]] extends BaseMapping[F]:
  object AtomRecordView extends TableDef("v_atom_record"):
    val Id: ColumnRef             = col("c_atom_id",          atom_id)
    val Instrument: ColumnRef     = col("c_instrument",       instrument)
    val AtomIndex: ColumnRef      = col("c_atom_index",       int4_pos)
    val ObservationId: ColumnRef  = col("c_observation_id",   observation_id)
    val SequenceType: ColumnRef   = col("c_sequence_type",    sequence_type)
    val VisitId: ColumnRef        = col("c_visit_id",         visit_id)
    val Description: ColumnRef    = col("c_description",      text_nonempty.opt)
    val ExecutionState: ColumnRef = col("c_execution_state",  atom_execution_state)
    val FirstEventTime: ColumnRef = col("c_first_event_time", core_timestamp.opt)
    val LastEventTime:  ColumnRef = col("c_last_event_time",  core_timestamp.opt)