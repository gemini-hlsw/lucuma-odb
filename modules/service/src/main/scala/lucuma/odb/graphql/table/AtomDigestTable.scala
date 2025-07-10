// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.atom_id
import lucuma.odb.util.Codecs.obs_class
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.time_span
import skunk.codec.boolean.bool
import skunk.codec.numeric.int2

trait AtomDigestTable[F[_]] extends BaseMapping[F]:

  object AtomDigestTable extends TableDef("t_atom_digest"):
    val AtomId: ColumnRef             = col("c_atom_id",                   atom_id)
    val AtomIndex: ColumnRef          = col("c_atom_index",                int2)
    val ObservationId: ColumnRef      = col("c_observation_id",            observation_id)

    val ObserveClass: ColumnRef       = col("c_observe_class",             obs_class)
    val NonChargedEstimate: ColumnRef = col("c_non_charged_time_estimate", time_span)
    val ProgramEstimate: ColumnRef    = col("c_program_time_estimate",     time_span)
    val HasArc: ColumnRef             = col("c_has_arc",                   bool)
    val HasFlat: ColumnRef            = col("c_has_flat",                  bool)