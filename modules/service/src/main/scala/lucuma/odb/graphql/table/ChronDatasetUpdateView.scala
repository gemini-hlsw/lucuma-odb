// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8

trait ChronDatasetUpdateView[F[_]] extends BaseMapping[F]:

  object ChronDatasetUpdateView extends TableDef("v_chron_dataset_update"):
    val ChronId       = col("c_chron_id",       int8)
    val Timestamp     = col("c_timestamp",      core_timestamp)
    val UserId        = col("c_user",           user_id.opt)
    val TransactionId = col("c_transaction_id", int8) // xid8
    
    val OperationId   = col("c_operation",      tg_op)
    val DatasetId     = col("c_dataset_id",     dataset_id)
    
    object Mod:
      val DatasetId     = col("c_mod_dataset_id",        bool)
      val StepId        = col("c_mod_step_id",           bool)
      val FileName      = col("c_mod_filename",          bool)
      val QaState       = col("c_mod_qa_state",          bool)
      val Interval      = col("c_mod_interval",          bool)
      val ObservationId = col("c_mod_observation_id",    bool)
      val VisitId       = col("c_mod_visit_id",          bool)
      val Reference     = col("c_mod_dataset_reference", bool)
      val Comment       = col("c_mod_comment",           bool)
      
    object Coalesce:
      val StartTime = col("c_coal_start_time", core_timestamp.opt)
      val EndTime   = col("c_coal_end_time",   core_timestamp.opt)

    object New:
      val DatasetId        = col("c_new_dataset_id",        dataset_id.opt)
      val StepId           = col("c_new_step_id",           step_id.opt)
      val FileName         = col("c_new_filename",          varchar_nonempty.opt)
      val QaState          = col("c_new_qa_state",          dataset_qa_state.opt)
      val ObservationId    = col("c_new_observation_id",    observation_id.opt)
      val VisitId          = col("c_new_visit_id",          visit_id.opt)
      val DatasetReference = col("c_new_dataset_reference", dataset_reference.opt)
      val Comment          = col("c_new_comment",           text_nonempty.opt)


