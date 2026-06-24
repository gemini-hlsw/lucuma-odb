// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import lucuma.odb.graphql.table.ProgramReferenceView
import lucuma.odb.graphql.table.ProgramView

trait SetProgramReferenceResultMapping[F[_]] extends BaseMapping[F]
                                                with ProgramReferenceView[F]
                                                with ProgramView[F] {

  lazy val SetProgramReferenceResultMapping: ObjectMapping =
    ObjectMapping(SetProgramReferenceResultType)(
      SqlField("programId", ProgramView.Id, key = true, hidden = true),
      SqlObject("reference", Join(ProgramView.Id, ProgramReferenceView.Id))
    )

}
