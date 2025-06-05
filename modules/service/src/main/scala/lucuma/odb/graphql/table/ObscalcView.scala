// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*
import skunk.circe.codec.all.*
import skunk.codec.numeric._int8

trait ObscalcView[F[_]] extends BaseMapping[F]:

  object ObscalcTable extends TableDef("v_obscalc"):
    val ProgramId: ColumnRef           = col("c_program_id",           program_id)
    val ObservationId: ColumnRef       = col("c_observation_id",       observation_id)
    val CalculationState: ColumnRef    = col("c_obscalc_state",        calculation_state)
    val LastInvitation: ColumnRef      = col("c_last_invalidation",    core_timestamp)
    val LastUpdate: ColumnRef          = col("c_last_update",          core_timestamp)
    val RetryAt: ColumnRef             = col("c_retry_at",             core_timestamp)
    val FailureCount: ColumnRef        = col("c_failure_count",        int4_nonneg)
    val OdbError: ColumnRef            = col("c_odb_error",            jsonb.opt)

    object Digest:
      val Id: ColumnRef                = col("c_exe_digest_id",        observation_id.embedded)
      val FullSetupTime: ColumnRef     = col("c_full_setup_time",      time_span.embedded)
      val ReacqSetupTime: ColumnRef    = col("c_reacq_setup_time",     time_span.embedded)

      case class SequenceDigest(abbr: String):
        val Id: ColumnRef              = col(s"c_${abbr}_digest_id",        observation_id.embedded)
        val ObsClass: ColumnRef        = col(s"c_${abbr}_obs_class",        obs_class.embedded)
        val NonChargedTime: ColumnRef  = col(s"c_${abbr}_non_charged_time", time_span.embedded)
        val ProgramTime: ColumnRef     = col(s"c_${abbr}_program_time",     time_span.embedded)
        val Offsets: ColumnRef         = col(s"c_${abbr}_offsets",          _int8.embedded)
        val AtomCount: ColumnRef       = col(s"c_${abbr}_atom_count",       int4_nonneg.embedded)
        val ExecutionState: ColumnRef  = col(s"c_${abbr}_execution_state",  execution_state.embedded)

      val Acquisition: SequenceDigest  = SequenceDigest("acq")
      val Science: SequenceDigest      = SequenceDigest("sci")

    object Workflow:
      val State: ColumnRef             = col("c_workflow_state",       observation_workflow_state)
      val Transitions: ColumnRef       = col("c_workflow_transitions", _observation_workflow_state)
      val Validations: ColumnRef       = col("c_workflow_validations", jsonb)