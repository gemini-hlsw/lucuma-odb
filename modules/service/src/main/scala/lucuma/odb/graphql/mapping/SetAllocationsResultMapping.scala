// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mapping

import grackle.skunk.SkunkMapping
import lucuma.odb.graphql.table.AllocationTable

trait SetAllocationsResultMapping[F[_]] extends AllocationTable[F] {

  lazy val SetAllocationsResultMapping =
    ObjectMapping(SetAllocationsResultType)(
      SqlField("programId", AllocationTable.ProgramId, key = true, hidden = true),
      SqlObject("allocations")
    )

  // N.B. I wasn't able to get allocations sorted.  Inexplicably adding an
  // elaborator generates an assertion error in Grackle SqlMapping (line 2040
  // as of the time of this writing).  Still, I hope to some day make this
  // possible so I'll leave the elaborator commented out for now.

  // Make sure the allocations are ordered by band + partner.
//  lazy val SetAllocationsResultElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] =
//    case (SetAllocationsResultType, "allocations", Nil) =>
//      Elab.transformChild { child =>
//        OrderBy(OrderSelections(List(
//          OrderSelection[ScienceBand](AllocationType / "scienceBand"),
//          OrderSelection[Partner](AllocationType / "partner")
//        )), child)
//      }

}

