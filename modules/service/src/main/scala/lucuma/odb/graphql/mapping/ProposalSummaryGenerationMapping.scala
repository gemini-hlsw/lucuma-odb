// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.SummaryGenerationView

trait ProposalSummaryGenerationMapping[F[_]] extends SummaryGenerationView[F]:

  lazy val ProposalSummaryGenerationMapping: ObjectMapping =
    ObjectMapping(ProposalSummaryGenerationType)(
      SqlField("synthetic_id", SummaryGenerationView.ProgramId, key = true, hidden = true),
      SqlField("state", SummaryGenerationView.State),
      SqlField("requestedAt", SummaryGenerationView.RequestedAt),
      SqlField("message", SummaryGenerationView.Message)
    )
