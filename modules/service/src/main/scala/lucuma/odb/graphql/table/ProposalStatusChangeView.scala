// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.table

import lucuma.odb.graphql.BaseMapping
import lucuma.odb.util.Codecs.*
import skunk.codec.numeric.int8

trait ProposalStatusChangeView[F[_]] extends BaseMapping[F]:

  object ProposalStatusChangeView extends TableDef("v_proposal_status_change"):
    val ChronId        = col("c_chron_id",        int8)
    val ProgramId      = col("c_program_id",      program_id)
    val Timestamp      = col("c_timestamp",       core_timestamp)
    val ProposalStatus = col("c_proposal_status", proposal_status)
