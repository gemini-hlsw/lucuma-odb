// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package table

import lucuma.odb.util.Codecs.*

trait TooTriggerTable[F[_]] extends BaseMapping[F]:

  object TooTriggerTable extends TableDef("t_too_trigger"):
    val Id               = col("c_too_trigger_id",    too_trigger_id)
    val ObservationId    = col("c_observation_id",    observation_id)
    val ProgramId        = col("c_program_id",        program_id)
    val Status           = col("c_status",            too_trigger_status)
    val TooActivation    = col("c_too_activation",    too_activation)
    val Supersedes       = col("c_supersedes",        too_trigger_id.opt)
    val ResolutionReason = col("c_resolution_reason", text_nonempty.opt)
    val RequestedAt      = col("c_requested_at",      core_timestamp)
    val RequestedBy      = col("c_requested_by",      user_id.opt)
    val UpdatedAt        = col("c_updated_at",        core_timestamp)
