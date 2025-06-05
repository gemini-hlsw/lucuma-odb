// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import grackle.skunk.SkunkMapping
import lucuma.odb.util.Codecs.*

trait ProposalView[F[_]] extends BaseMapping[F] {

  object ProposalView extends TableDef("v_proposal") {

    val ProgramId       = col("c_program_id",      program_id)
    val ScienceSubtype  = col("c_science_subtype", science_subtype)

    val Title           = col("c_title",    text_nonempty.opt)
    val Abstract        = col("c_abstract", text_nonempty.opt)
    val Category        = col("c_category", tag.opt)

    val TooActivation   = col("c_too_activation", too_activation)
    val MinPercent      = col("c_min_percent",    int_percent)

    val CallId          = col("c_cfp_id", cfp_id.opt)

    object Classical {
      val Id = col("c_program_id_c", program_id.embedded)
    }
    object DemoScience {
      val Id = col("c_program_id_s", program_id.embedded)
    }
    object DirectorsTime {
      val Id = col("c_program_id_d", program_id.embedded)
    }
    object FastTurnaround {
      val Id          = col("c_program_id_f", program_id.embedded)
      val PiAffiliate = col("c_ft_partner", partner.opt)
      val ReviewerId = col("c_reviewer_id", program_user_id.opt)
      val MentorId   = col("c_mentor_id", program_user_id.opt)
    }
    object LargeProgram {
      val Id              = col("c_program_id_l", program_id.embedded)
      val MinPercentTotal = col("c_min_percent_total", int_percent)
      val TotalTime       = col("c_total_time", time_span)
    }
    object PoorWeather {
      val Id  = col("c_program_id_p", program_id.embedded)
    }
    object Queue {
      val Id = col("c_program_id_q", program_id.embedded)
    }
    object SystemVerification {
      val Id = col("c_program_id_v", program_id.embedded)
    }
  }

}
