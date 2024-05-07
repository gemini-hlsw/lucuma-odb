// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.AllocationTable

trait SetAllocationResultMapping[F[_]] extends AllocationTable[F] {

  lazy val SetAllocationResultMapping =
    ObjectMapping(
      tpe = SetAllocationResultType,
      fieldMappings = List(
        SqlField("programId", AllocationTable.ProgramId, key = true, hidden = true),
        SqlField("partner", AllocationTable.Partner, key = true, hidden = true),
        SqlObject("allocation"),
      )
    )

}

