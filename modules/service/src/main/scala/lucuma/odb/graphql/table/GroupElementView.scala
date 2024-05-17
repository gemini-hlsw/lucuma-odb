// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait GroupElementView[F[_]] extends BaseMapping[F] {

  object GroupElementView extends TableDef("v_group_element") {
    val Id                 = col("c_group_element_id", text)
    val ProgramId          = col("c_program_id", program_id)
    val GroupId            = col("c_group_id", group_id.opt)
    val Index              = col("c_index", int2_nonneg)
    val ChildGroupId       = col("c_child_group_id", group_id.opt)
    val ChildObservationId = col("c_child_observation_id", observation_id.opt)
    val Existence          = col("c_existence", existence)
  }

}