// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.ProposalTable

trait CreateProposalResultMapping[F[_]] extends ProposalTable[F] {

  lazy val CreateProposalResultMapping = 
    ObjectMapping(
      tpe = CreateProposalResultType,
      fieldMappings = List(
        SqlField("programId", ProposalTable.ProgramId, key = true),
        SqlObject("proposal")
      )
    )
}
