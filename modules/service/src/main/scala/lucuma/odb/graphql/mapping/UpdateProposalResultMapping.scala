// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ProposalView

trait UpdateProposalResultMapping[F[Unit]] extends ProposalView[F] {

  lazy val UpdateProposalResultMapping =
    ObjectMapping(UpdateProposalResultType)(
      SqlField("programId", ProposalView.ProgramId, key = true, hidden = true),
      SqlObject("proposal")
    )
  
}
