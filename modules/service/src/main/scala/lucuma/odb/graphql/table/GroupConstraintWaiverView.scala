// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool
import skunk.codec.temporal.date

trait GroupConstraintWaiverView[F[_]] extends BaseMapping[F]:

  // The mutation results are keyed by the table and join the view for the
  // waiver: Grackle reads a joined table's columns as nullable, as the
  // nullable embedded objects need.
  object GroupConstraintWaiverTable extends TableDef("t_group_constraint_waiver"):
    val Id: ColumnRef = col("c_waiver_id", group_constraint_waiver_id)

  object GroupConstraintWaiverView extends TableDef("v_group_constraint_waiver"):
    val Id: ColumnRef                    = col("c_waiver_id",              group_constraint_waiver_id)
    val GroupId: ColumnRef               = col("c_group_id",               group_id)
    val Code: ColumnRef                  = col("c_code",                   group_constraint_violation_code)
    val ElementGroupId: ColumnRef        = col("c_element_group_id",       group_id.opt)
    val ElementObservationId: ColumnRef  = col("c_element_observation_id", observation_id.opt)
    // The element ids again, for comparing in filters (see WhereGroupElementId).
    val ElementGroupIdValue: ColumnRef       = col("c_element_group_id",       group_id)
    val ElementObservationIdValue: ColumnRef = col("c_element_observation_id", observation_id)
    val NightSite: ColumnRef             = col("c_night_site",             site)
    val NightDate: ColumnRef             = col("c_night_date",             date)
    val GrantedBy: ColumnRef             = col("c_granted_by",             user_id)
    val GrantedAt: ColumnRef             = col("c_granted_at",             core_timestamp)
    val GrantedReason: ColumnRef         = col("c_granted_reason",         text_nonempty.opt)
    val RevokedBy: ColumnRef             = col("c_revoked_by",             user_id)
    val RevokedAt: ColumnRef             = col("c_revoked_at",             core_timestamp)
    val RevokedReason: ColumnRef         = col("c_revoked_reason",         text_nonempty.opt)
    val ElementId: ColumnRef             = col("c_element_id",             group_constraint_waiver_id)
    val NightId: ColumnRef               = col("c_night_id",               group_constraint_waiver_id)
    val RevokedId: ColumnRef             = col("c_revoked_id",             group_constraint_waiver_id)
    val SubgroupId: ColumnRef            = col("c_subgroup_id",            group_id.opt)
    val Active: ColumnRef                = col("c_active",                 bool)
