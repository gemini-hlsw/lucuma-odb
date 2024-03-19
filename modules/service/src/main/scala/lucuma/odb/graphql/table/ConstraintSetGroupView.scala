// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait ConstraintSetGroupView[F[_]] extends BaseMapping[F] {

    object ConstraintSetGroupView extends TableDef("v_constraint_set_group") {
      val ProgramId: ColumnRef = col("c_program_id", program_id)
      val ConstraintSetKey: ColumnRef = col("c_conditions_key", text)
      val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    }

}