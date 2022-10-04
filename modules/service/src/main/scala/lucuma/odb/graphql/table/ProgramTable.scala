// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._

trait ProgramTable[F[_]] extends BaseMapping[F] {

  object ProgramTable extends TableDef("t_program") {
    val Id        = col("c_program_id", program_id)
    val PiUserId  = col("c_pi_user_id", user_id)
    val Existence = col("c_existence", existence)
    val Name      = col("c_name", text_nonempty.opt)
    object PlannedTime {
      val Pi        = col("c_pts_pi", interval)
      val Uncharged = col("c_pts_uncharged", interval)
      val Execution = col("c_pts_execution", interval)
    }
  }

}