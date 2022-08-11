// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.AllocationTable

import edu.gemini.grackle.skunk.SkunkMapping
import lucuma.odb.graphql.snippet.table.AllocationTable

trait SetAllocationResultMapping[F[_]]
  extends AllocationTable[F] { this: SkunkMapping[F] =>

  lazy val SetAllocationResultType = schema.ref("SetAllocationResult")

  lazy val SetAllocationResultMapping =
    ObjectMapping(
      tpe = SetAllocationResultType,
      fieldMappings = List(
        SqlField("programId", AllocationTable.ProgramId, key = true, hidden = true),
        SqlField("partner", AllocationTable.Partner, key = true),
        SqlObject("allocation"),
      )
    )

}

