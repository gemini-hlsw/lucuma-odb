// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.SummaryFailureView
import lucuma.odb.graphql.table.SummaryGenerationView

trait ProposalSummaryGenerationMapping[F[_]] extends SummaryGenerationView[F] with SummaryFailureView[F]:

  lazy val ProposalSummaryGenerationMapping: ObjectMapping =
    ObjectMapping(ProposalSummaryGenerationType)(
      SqlField("synthetic_id", SummaryGenerationView.ProgramId, key = true, hidden = true),
      SqlField("state", SummaryGenerationView.State),
      SqlField("requestedAt", SummaryGenerationView.RequestedAt),
      SqlObject("failures", Join(SummaryGenerationView.ProgramId, SummaryFailureView.ProgramId))
    )

  lazy val ProposalSummaryFailureMapping: ObjectMapping =
    ObjectMapping(ProposalSummaryFailureType)(
      SqlField("id", SummaryFailureView.Id, key = true, hidden = true),
      SqlField("partner", SummaryFailureView.Partner),
      SqlField("message", SummaryFailureView.Message)
    )
