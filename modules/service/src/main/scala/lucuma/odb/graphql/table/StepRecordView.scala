// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool

trait StepRecordView[F[_]] extends BaseMapping[F]:

  object StepRecordView extends TableDef("v_step_record"):
    val Id: ColumnRef             = col("c_step_id",          step_id)
    val AtomId: ColumnRef         = col("c_atom_id",          atom_id)
    val VisitId: ColumnRef        = col("c_visit_id",         visit_id)
    val StepIndex: ColumnRef      = col("c_step_index",       int4_pos)
    val Instrument: ColumnRef     = col("c_instrument",       instrument)
    val StepType: ColumnRef       = col("c_step_type",        step_type)
    val ObserveClass: ColumnRef   = col("c_observe_class",    obs_class)
    val ExecutionState: ColumnRef = col("c_execution_state",  step_execution_state)
    val TimeEstimate: ColumnRef   = col("c_time_estimate",    time_span)
    val Breakpoint: ColumnRef     = col("c_breakpoint",       breakpoint)
    val FirstEventTime: ColumnRef = col("c_first_event_time", core_timestamp)
    val LastEventTime:  ColumnRef = col("c_last_event_time",  core_timestamp)
    val OffsetP: ColumnRef        = col("c_offset_p",         angle_µas)
    val OffsetQ: ColumnRef        = col("c_offset_q",         angle_µas)
    val GuideState: ColumnRef     = col("c_guide_state",      guide_state)
    val QaState: ColumnRef        = col("c_qa_state",         dataset_qa_state.opt)

    object Gcal:
      val Continuum: ColumnRef = col("c_gcal_continuum", gcal_continuum.opt)
      val ArArc: ColumnRef     = col("c_gcal_ar_arc",    bool)
      val CuarArc: ColumnRef   = col("c_gcal_cuar_arc",  bool)
      val TharArc: ColumnRef   = col("c_gcal_thar_arc",  bool)
      val XeArc: ColumnRef     = col("c_gcal_xe_arc",    bool)

      val Filter: ColumnRef    = col("c_gcal_filter",    gcal_filter)
      val Diffuser: ColumnRef  = col("c_gcal_diffuser",  gcal_diffuser)
      val Shutter: ColumnRef   = col("c_gcal_shutter",   gcal_shutter)

    object SmartGcal:
      val Type: ColumnRef = col("c_smart_gcal_type", smart_gcal_type)