// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.angle_µas
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.offset_generator_role
import lucuma.odb.util.Codecs.telescope_config_generator_type
import skunk.codec.numeric.int8

trait TelescopeConfigGeneratorView[F[_]] extends BaseMapping[F]:

  object TelescopeConfigGeneratorView extends TableDef("v_offset_generator"):

    val ObservationId: ColumnRef       = col("c_observation_id", observation_id)
    val ObjectObservationId: ColumnRef = col("c_object_observation_id", observation_id.embedded)
    val SkyObservationId: ColumnRef    = col("c_sky_observation_id", observation_id.embedded)

    val TelescopeConfigGeneratorRole: ColumnRef = col("c_role", offset_generator_role)
    val Seed: ColumnRef                = col("c_seed",                  int8)

    object Enumerated:
      val ObservationId: ColumnRef       = col("c_enumerated_observation_id", observation_id.embedded)
      val TelescopeConfigGeneratorRole: ColumnRef = col("c_enumerated_role", offset_generator_role.embedded)

    object Random:
      val ObservationId: ColumnRef       = col("c_random_observation_id", observation_id.embedded)
      val TelescopeConfigGeneratorRole: ColumnRef = col("c_random_role", offset_generator_role.embedded)

    object Spiral:
      val ObservationId: ColumnRef       = col("c_spiral_observation_id", observation_id.embedded)
      val TelescopeConfigGeneratorRole: ColumnRef = col("c_spiral_role", offset_generator_role.embedded)

    object Uniform:
      val ObservationId: ColumnRef       = col("c_uniform_observation_id", observation_id.embedded)
      val TelescopeConfigGeneratorRole: ColumnRef = col("c_uniform_role", offset_generator_role.embedded)

    val TelescopeConfigGeneratorType: ColumnRef = col("c_type", telescope_config_generator_type)

    val UniformCornerAP: ColumnRef       = col("c_uniform_corner_a_p", angle_µas)
    val UniformCornerAQ: ColumnRef       = col("c_uniform_corner_a_q", angle_µas)
    val UniformCornerBP: ColumnRef       = col("c_uniform_corner_b_p", angle_µas)
    val UniformCornerBQ: ColumnRef       = col("c_uniform_corner_b_q", angle_µas)

    val Size: ColumnRef          = col("c_size", angle_µas)
    val CenterOffsetP: ColumnRef = col("c_center_offset_p", angle_µas)
    val CenterOffsetQ: ColumnRef = col("c_center_offset_q", angle_µas)