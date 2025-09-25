// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.atom_execution_state
import lucuma.odb.util.Codecs.atom_id
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.idempotency_key
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.sequence_type
import lucuma.odb.util.Codecs.visit_id

trait AtomRecordTable[F[_]] extends BaseMapping[F]:
  object AtomRecordTable extends TableDef("t_atom_record"):
    val Id: ColumnRef             = col("c_atom_id",         atom_id)
    val Instrument: ColumnRef     = col("c_instrument",      instrument)
    val VisitId: ColumnRef        = col("c_visit_id",        visit_id)
    val ObservationId: ColumnRef  = col("c_observation_id",  observation_id)
    val SequenceType: ColumnRef   = col("c_sequence_type",   sequence_type)
    val Created: ColumnRef        = col("c_created",         core_timestamp)
    val ExecutionState: ColumnRef = col("c_execution_state", atom_execution_state)
    val GeneratedId: ColumnRef    = col("c_generated_id",    atom_id.opt)
    val IdempotencyKey: ColumnRef = col("c_idempotency_key", idempotency_key.opt)