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

    object Digest:
      val FullSetupTime: ColumnRef     = col("c_full_setup_time",      time_span)
      val ReacqSetupTime: ColumnRef    = col("c_reacq_setup_time",     time_span)

      case class SequenceDigest(abbr: String):
        val ObsClass: ColumnRef        = col(s"c_${abbr}_obs_class",        obs_class)
        val NonChargedTime: ColumnRef  = col(s"c_${abbr}_non_charged_time", time_span)
        val ProgramTime: ColumnRef     = col(s"c_${abbr}_program_time",     time_span)
        val Offsets: ColumnRef         = col(s"c_${abbr}_offsets",          offset_array)
        val AtomCount: ColumnRef       = col(s"c_${abbr}_atom_count",       int4_nonneg)
        val ExecutionState: ColumnRef  = col(s"c_${abbr}_execution_state",  execution_state)

      val Acquisition: SequenceDigest  = SequenceDigest("acq")
      val Science: SequenceDigest      = SequenceDigest("sci")

    object Workflow:
      val State: ColumnRef             = col("c_workflow_state",       observation_workflow_state)
      val Transitions: ColumnRef       = col("c_workflow_transitions", _observation_workflow_state)
      val Validations: ColumnRef       = col("c_workflow_validations", jsonb)