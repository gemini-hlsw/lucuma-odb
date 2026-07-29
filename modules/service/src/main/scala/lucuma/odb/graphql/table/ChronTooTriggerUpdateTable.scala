// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8

trait ChronTooTriggerUpdateTable[F[_]] extends BaseMapping[F]:

  object ChronTooTriggerUpdateTable extends TableDef("t_chron_too_trigger_update"):
    val ChronId       = col("c_chron_id",       int8)
    val Timestamp     = col("c_timestamp",      core_timestamp)
    val UserId        = col("c_user",           user_id.opt)
    val TransactionId = col("c_transaction_id", int8) // xid8
    val OperationId   = col("c_operation",      tg_op)
    val TooTriggerId  = col("c_too_trigger_id", too_trigger_id)

    object Mod:
      val ObservationId    = col("c_mod_observation_id",    bool)
      val ProgramId        = col("c_mod_program_id",        bool)
      val Status           = col("c_mod_status",            bool)
      val ResolutionReason = col("c_mod_resolution_reason", bool)

    object New:
      val ObservationId    = col("c_new_observation_id",    observation_id.opt)
      val ProgramId        = col("c_new_program_id",        program_id.opt)
      val Status           = col("c_new_status",            too_trigger_status.opt)
      val ResolutionReason = col("c_new_resolution_reason", text_nonempty.opt)
