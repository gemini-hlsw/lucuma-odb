// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package snippet
package mapping

import table.AllocationTable

import edu.gemini.grackle.skunk.SkunkMapping

trait AllocationMapping[F[_]]
  extends AllocationTable[F] { this: SkunkMapping[F] =>

  lazy val AllocationType = schema.ref("Allocation")

  lazy val AllocationMapping =
    ObjectMapping(
      tpe = AllocationType,
      fieldMappings = List(
        SqlField("programId", AllocationTable.ProgramId, key = true, hidden = true),
        SqlField("partner", AllocationTable.Partner, key = true),
        SqlObject("duration"),
      )
    )

}

