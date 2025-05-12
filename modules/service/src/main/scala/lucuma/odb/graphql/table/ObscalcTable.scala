// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.circe.codec.all.*

trait ObscalcTable[F[_]] extends BaseMapping[F]:

  object ObscalcTable extends TableDef("t_obscalc"):
    val ProgramId: ColumnRef           = col("c_program_id",           program_id)
    val ObservationId: ColumnRef       = col("c_observation_id",       observation_id)
    val CalculationState: ColumnRef    = col("c_obscalc_state",        calculation_state)
    val LastInvitation: ColumnRef      = col("c_last_invalidation",    core_timestamp)
    val LastUpdate: ColumnRef          = col("c_last_update",          core_timestamp)
    val RetryAt: ColumnRef             = col("c_retry_at",             core_timestamp)
    val FailureCount: ColumnRef        = col("c_failure_count",        int4_nonneg)
    val OdbError: ColumnRef            = col("c_odb_error",            jsonb.opt)