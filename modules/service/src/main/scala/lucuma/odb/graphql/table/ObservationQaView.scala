// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*

trait ObservationQaView[F[_]] extends BaseMapping[F]:

  object ObservationQaView extends TableDef("v_observation_qa"):
    val ObservationId: ColumnRef = col("c_observation_id", observation_id)
    val State: ColumnRef         = col("c_state",          observation_qa_state.opt)
    val Computed: ColumnRef      = col("c_computed",       observation_qa_state.opt)
    val Override: ColumnRef      = col("c_override",       observation_qa_state.opt)

    object Datasets:
      val Total: ColumnRef       = col("c_total",          int4_nonneg)
      val Pass: ColumnRef        = col("c_pass",           int4_nonneg)
      val Usable: ColumnRef      = col("c_usable",         int4_nonneg)
      val Fail: ColumnRef        = col("c_fail",           int4_nonneg)
      val NotAssessed: ColumnRef = col("c_not_assessed",   int4_nonneg)
