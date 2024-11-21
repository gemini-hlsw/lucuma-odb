// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import skunk.codec.text.text

trait ObservingModeGroupView[F[_]] extends BaseMapping[F]:

  object ObservingModeGroupView extends TableDef("v_observing_mode_group"):
    val ProgramId: ColumnRef        = col("c_program_id",     program_id)
    val ObservingModeKey: ColumnRef = col("c_mode_key",       text)
    val ObservationId: ColumnRef    = col("c_observation_id", observation_id)