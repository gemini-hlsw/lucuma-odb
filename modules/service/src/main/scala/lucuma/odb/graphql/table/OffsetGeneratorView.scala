// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.angle_µas
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.offset_generator_role
import lucuma.odb.util.Codecs.offset_generator_type

trait OffsetGeneratorView[F[_]] extends BaseMapping[F]:

  object OffsetGeneratorView extends TableDef("v_offset_generator"):

    val ObservationId: ColumnRef       = col("c_observation_id", observation_id)
    val ObjectObservationId: ColumnRef = col("c_object_observation_id", observation_id.opt)
    val SkyObservationId: ColumnRef    = col("c_sky_observation_id", observation_id.opt)

    val OffsetGeneratorRole: ColumnRef = col("c_role", offset_generator_role)

    object Enumerated:
      val ObservationId: ColumnRef       = col("c_enumerated_observation_id", observation_id.embedded)
      val OffsetGeneratorRole: ColumnRef = col("c_enumerated_role", offset_generator_role.embedded)

    object Grid:
      val ObservationId: ColumnRef       = col("c_grid_observation_id", observation_id.embedded)
      val OffsetGeneratorRole: ColumnRef = col("c_grid_role", offset_generator_role.embedded)

    object Random:
      val ObservationId: ColumnRef       = col("c_random_observation_id", observation_id.embedded)
      val OffsetGeneratorRole: ColumnRef = col("c_random_role", offset_generator_role.embedded)

    object Spiral:
      val ObservationId: ColumnRef       = col("c_spiral_observation_id", observation_id.embedded)
      val OffsetGeneratorRole: ColumnRef = col("c_spiral_role", offset_generator_role.embedded)

    val OffsetGeneratorType: ColumnRef = col("c_type", offset_generator_type)

    val GridCornerAP: ColumnRef        = col("c_grid_corner_a_p", angle_µas)
    val GridCornerAQ: ColumnRef        = col("c_grid_corner_a_q", angle_µas)
    val GridCornerBP: ColumnRef        = col("c_grid_corner_b_p", angle_µas)
    val GridCornerBQ: ColumnRef        = col("c_grid_corner_b_q", angle_µas)

    val Size: ColumnRef                = col("c_size", angle_µas)
    val CenterOffsetP: ColumnRef       = col("c_center_offset_p", angle_µas)
    val CenterOffsetQ: ColumnRef       = col("c_center_offset_q", angle_µas)