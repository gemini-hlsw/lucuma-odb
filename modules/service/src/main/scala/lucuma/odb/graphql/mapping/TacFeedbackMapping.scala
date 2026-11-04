// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import table.TacFeedbackTable

trait TacFeedbackMapping[F[_]] extends TacFeedbackTable[F]:

  lazy val TacFeedbackMapping =
    ObjectMapping(TacFeedbackType)(
      SqlField("programId", TacFeedbackTable.ProgramId, key = true, hidden = true),
      SqlField("partner",   TacFeedbackTable.Partner,   key = true),
      SqlField("decision",  TacFeedbackTable.Decision),
      SqlField("ranking",   TacFeedbackTable.Ranking),
      SqlObject("recommendedTime"),
      SqlField("comments",  TacFeedbackTable.Comments)
    )
