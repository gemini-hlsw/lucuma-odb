// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._

trait ProgramTable[F[_]] extends BaseMapping[F] {

  object ProgramTable extends TableDef("t_program") {
    val Id             = col("c_program_id", program_id)
    val PiUserId       = col("c_pi_user_id", user_id)
    val Existence      = col("c_existence", existence)
    val Name           = col("c_name", text_nonempty.opt)
    val ProposalStatus = col("c_proposal_status", tag)
    object PlannedTime {
      val Pi        = col("c_pts_pi", time_span)
      val Uncharged = col("c_pts_uncharged", time_span)
      val Execution = col("c_pts_execution", time_span)
    }
    object Reference {
      val Semester         = col("c_semester",          semester.opt)
      val SemesterIndex    = col("c_semester_index",    int4_pos.opt)
      val ProgramReference = col("c_program_reference", program_reference.opt)
    }
  }

}
