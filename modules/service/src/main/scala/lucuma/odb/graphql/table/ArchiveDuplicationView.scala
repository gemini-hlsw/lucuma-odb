// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool

trait ArchiveDuplicationView[F[_]] extends BaseMapping[F]:

  object ArchiveDuplicationView extends TableDef("v_archive_duplication"):
    val ObservationId: ColumnRef    = col("c_observation_id",  observation_id)
    val State: ColumnRef            = col("c_state",           archive_duplication_state)
    val MatchCount: ColumnRef       = col("c_match_count",     int4_nonneg)
    val Saturated: ColumnRef        = col("c_saturated",       bool)
    val LastCheckedAt: ColumnRef    = col("c_last_checked_at", core_timestamp.opt)
    val LastAttemptedAt: ColumnRef  = col("c_last_attempted_at", core_timestamp.opt)
    val Stale: ColumnRef            = col("c_stale",           bool)
    val Error: ColumnRef            = col("c_error",           text_nonempty.opt)
    val SearchTargetName: ColumnRef = col("c_search_target",   text_nonempty.opt)
    val QueryUrls: ColumnRef        = col("c_query_urls",      text_list)

    object SearchCoordinates:
      val SyntheticId: ColumnRef  = col("c_search_center_id", observation_id.embedded)
      val Ra: ColumnRef           = col("c_search_ra",        right_ascension.embedded)
      val Dec: ColumnRef          = col("c_search_dec",       declination.embedded)

    object SearchRadius:
      val SyntheticId: ColumnRef  = col("c_search_radius_id", observation_id.embedded)
      val Value: ColumnRef        = col("c_search_radius",    angle_µas.embedded)
