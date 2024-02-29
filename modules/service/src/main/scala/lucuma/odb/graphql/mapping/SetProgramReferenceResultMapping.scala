// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProgramReferenceView
import lucuma.odb.graphql.table.ProgramTable

trait SetProgramReferenceResultMapping[F[_]] extends BaseMapping[F]
                                                with ProgramReferenceView[F]
                                                with ProgramTable[F] {

  lazy val SetProgramReferenceResultMapping: ObjectMapping =
    ObjectMapping(
      tpe = SetProgramReferenceResultType,
      fieldMappings = List(
        SqlField("programId", ProgramTable.Id, key = true, hidden = true),
        SqlObject("reference", Join(ProgramTable.Id, ProgramReferenceView.Id))
      )
    )

}
