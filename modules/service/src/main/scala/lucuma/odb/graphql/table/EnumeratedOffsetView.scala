// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.angle_µas
import lucuma.odb.util.Codecs.guide_state
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.offset_generator_role
import skunk.codec.numeric.int4

trait EnumeratedOffsetView[F[_]] extends BaseMapping[F]:

  object EnumeratedOffsetView extends TableDef("v_enumerated_offset"):
    val ObservationId: ColumnRef       = col("c_observation_id", observation_id)
    val ObjectObservationId: ColumnRef = col("c_object_observation_id", observation_id.embedded)
    val SkyObservationId: ColumnRef    = col("c_sky_observation_id", observation_id.embedded)

    val OffsetGeneratorRole: ColumnRef = col("c_role", offset_generator_role)
    val Index: ColumnRef               = col("c_index", int4)
    val OffsetP: ColumnRef             = col("c_offset_p", angle_µas)
    val OffsetQ: ColumnRef             = col("c_offset_q", angle_µas)
    val GuideState: ColumnRef          = col("c_guide_state", guide_state)