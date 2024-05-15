// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.graphql.table.CallForProposalsView
import lucuma.odb.graphql.table.PartnerSplitTable
import lucuma.odb.graphql.table.ProgramTable
import lucuma.odb.graphql.table.ProposalReferenceView
import lucuma.odb.graphql.table.ProposalView

trait ProposalMapping[F[_]] extends PartnerSplitTable[F]
                               with CallForProposalsView[F]
                               with Predicates[F]
                               with ProgramTable[F]
                               with ProposalReferenceView[F]
                               with ProposalView[F]
                               with KeyValueEffectHandler[F] {

  lazy val ProposalMapping =
    ObjectMapping(ProposalType)(
      SqlField("program_id", ProposalView.ProgramId, key = true, hidden = true),
      SqlObject("reference", Join(ProposalView.ProgramId, ProposalReferenceView.Id)),
      SqlObject("call", Join(ProposalView.CallId, CallForProposalsView.Id)),
      SqlField("title", ProposalView.Title),
      SqlField("category", ProposalView.Category),
      SqlField("abstract", ProposalView.Abstract),
      SqlObject("type")
    )

}

