// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ProposalView

trait CreateProposalResultMapping[F[_]] extends ProposalView[F] {

  lazy val CreateProposalResultMapping = 
    ObjectMapping(CreateProposalResultType)(
      SqlField("programId", ProposalView.ProgramId, key = true, hidden = true),
      SqlObject("proposal")
    )

}
