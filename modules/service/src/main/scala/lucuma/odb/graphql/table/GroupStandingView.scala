// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*

trait GroupStandingView[F[_]] extends BaseMapping[F]:

  object GroupStandingView extends TableDef("v_group_standing"):
    val GroupId: ColumnRef          = col("c_group_id",          group_id)
    val Standing: ColumnRef         = col("c_standing",          group_standing_status)
    val DecidingGroupId: ColumnRef  = col("c_deciding_group_id", group_id.opt)
    val CalculationState: ColumnRef = col("c_calc_state",        calculation_state)

  object ObservationGroupStandingView extends TableDef("v_observation_group_standing"):
    val ObservationId: ColumnRef    = col("c_observation_id",    observation_id)
    val Standing: ColumnRef         = col("c_standing",          group_standing_status)
    val DecidingGroupId: ColumnRef  = col("c_deciding_group_id", group_id.opt)
    val CalculationState: ColumnRef = col("c_calc_state",        calculation_state)
