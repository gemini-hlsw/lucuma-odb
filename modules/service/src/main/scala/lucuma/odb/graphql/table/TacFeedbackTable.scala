// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package table

import lucuma.odb.util.Codecs.*

trait TacFeedbackTable[F[_]] extends BaseMapping[F]:

  object TacFeedbackTable extends TableDef("t_tac_feedback"):
    val ProgramId       = col("c_program_id",       program_id)
    val Partner         = col("c_partner",          partner)
    val Decision        = col("c_decision",         tac_partner_decision)
    val Ranking         = col("c_ranking",          numeric_pos)
    val Comments        = col("c_comments",         text_nonempty.opt)

  object TacRecommendedTimeTable extends TableDef("t_tac_recommended_time"):
    val ProgramId       = col("c_program_id",       program_id)
    val Partner         = col("c_partner",          partner)
    val ScienceBand     = col("c_science_band",     science_band)
    val RecommendedTime = col("c_recommended_time", time_span)
