// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping

import table.AllocationTable

trait AllocationMapping[F[_]] extends AllocationTable[F]  {

  lazy val AllocationMapping =
    ObjectMapping(AllocationType)(
      SqlField("programId",   AllocationTable.ProgramId,   key = true, hidden = true),
      SqlField("category",    AllocationTable.Category,    key = true),
      SqlField("scienceBand", AllocationTable.ScienceBand, key = true),
      SqlObject("duration"),
    )

}

