// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs._
import skunk.codec.all._
import skunk.data.Type

trait ProposalTable[F[_]] { self: SkunkMapping[F] =>

  object ProposalTable extends TableDef("t_proposal") {
    val ProgramId       = col("c_program_id", program_id)
    val Title           = col("c_title", text_nonempty.opt)
    val Abstrakt        = col("c_abstract", text_nonempty.opt)
    val Category        = col("c_category", tag.opt)
    val TooActivation   = col("c_too_activation", too_activation.opt)
    val Clazz           = col("c_class", tag)
    val MinPercent      = col("c_min_percent", int_percent)
    val MinPercentTotal = col("c_min_percent_total", int_percent.embedded)
    val TotalTime       = col("c_totaltime", interval.embedded)
  }

}