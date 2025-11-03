// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*

trait GroupView[F[_]] extends BaseMapping[F] {

  object GroupView extends TableDef("v_group") {
    val Id            = col("c_group_id", group_id)
    val ProgramId     = col("c_program_id", program_id)
    val ParentId      = col("c_parent_id", group_id.opt)
    val ParentIndex   = col("c_parent_index", int2_nonneg)
    val Name          = col("c_name", text_nonempty.opt)
    val Description   = col("c_description", text_nonempty.opt)
    val MinRequired   = col("c_min_required", int2.opt)
    val Ordered       = col("c_ordered", bool)
    val MinInterval   = col("c_min_interval", time_span.embedded)
    val MaxInterval   = col("c_max_interval", time_span.embedded)
    val MinIntervalId = col("c_min_interval_id", group_id.embedded)
    val MaxIntervalId = col("c_max_interval_id", group_id.embedded)
    val Existence        = col("c_existence", existence)
    val System           = col("c_system", bool)
    val CalibrationRoles = col("c_calibration_roles", _calibration_role)
  }

}
