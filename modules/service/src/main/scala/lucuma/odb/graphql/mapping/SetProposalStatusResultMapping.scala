// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.ProgramTable

trait SetProposalStatusResultMapping[F[_]] extends ProgramTable[F] {
  
  lazy val SetProposalStatusResultMapping =
    ObjectMapping(SetProposalStatusResultType)(
      SqlField("programId", ProgramTable.Id, key = true, hidden = true),
      SqlObject("program")
    )

}
