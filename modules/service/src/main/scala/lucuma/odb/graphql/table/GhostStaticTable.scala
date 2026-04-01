// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.instrument
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.GhostCodecs.ghost_resolution_mode
import skunk.codec.numeric.int8

trait GhostStaticTable[F[_]] extends BaseMapping[F]:

  object GhostStaticTable extends TableDef("t_ghost_static"):
    val Id: ColumnRef             = col("c_static_id",       int8)
    val ObservationId: ColumnRef  = col("c_observation_id",  observation_id)
    val Instrument: ColumnRef     = col("c_instrument",      instrument)
    val ResolutionMode: ColumnRef = col("c_resolution_mode", ghost_resolution_mode)