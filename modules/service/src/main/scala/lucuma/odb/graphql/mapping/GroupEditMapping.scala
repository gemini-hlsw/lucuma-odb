// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.GroupView
import lucuma.odb.graphql.table.ProgramTable


trait GroupEditMapping[F[_]] extends GroupView[F] with ProgramTable[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val GroupEditMapping: ObjectMapping =
    ObjectMapping(GroupEditType)(
      SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
      CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
      SqlObject("program"),
      SqlObject("value", Join(ProgramTable.Id, GroupView.ProgramId))
    )

}
