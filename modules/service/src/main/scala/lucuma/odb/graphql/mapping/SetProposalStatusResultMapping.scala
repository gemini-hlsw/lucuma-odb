// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import lucuma.odb.graphql.table.ProgramView

trait SetProposalStatusResultMapping[F[_]] extends ProgramView[F] {
  
  lazy val SetProposalStatusResultMapping =
    ObjectMapping(SetProposalStatusResultType)(
      SqlField("programId", ProgramView.Id, key = true, hidden = true),
      SqlObject("program")
    )

}
