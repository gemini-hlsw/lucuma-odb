// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.atom_id
import lucuma.odb.util.Codecs.core_timestamp
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.int2_nonneg
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.sequence_type
import lucuma.odb.util.Codecs.visit_id

trait AtomRecordTable[F[_]] extends BaseMapping[F] {

  object AtomRecordTable extends TableDef("t_atom_record") {
    val Id: ColumnRef            = col("c_atom_id",        atom_id)
    val Instrument: ColumnRef    = col("c_instrument",     instrument)
    val VisitId: ColumnRef       = col("c_visit_id",       visit_id)
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val StepCount: ColumnRef     = col("c_step_count",     int2_nonneg)
    val SequenceType: ColumnRef  = col("c_sequence_type",  sequence_type)
    val Created: ColumnRef       = col("c_created",        core_timestamp)
  }

}
