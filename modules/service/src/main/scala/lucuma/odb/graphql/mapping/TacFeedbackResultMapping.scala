// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import table.TacFeedbackTable

trait TacFeedbackResultMapping[F[_]] extends TacFeedbackTable[F]:

  lazy val TacFeedbackResultMapping =
    ObjectMapping(TacFeedbackResultType)(
      SqlField("programId", TacFeedbackTable.ProgramId, key = true, hidden = true),
      SqlField("partner",   TacFeedbackTable.Partner,   key = true, hidden = true),
      SqlObject("tacFeedback")
    )
