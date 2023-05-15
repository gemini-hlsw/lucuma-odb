// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import edu.gemini.grackle.Result
import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.GroupView


trait GroupEditMapping[F[_]] extends GroupView[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val GroupEditMapping: ObjectMapping =
    ObjectMapping(
      tpe = GroupEditType,
      fieldMappings = List(
        SqlField("synthetic-id", GroupView.Id, key = true, hidden = true),
        CursorField("id", _ => Result(0L), List("synthetic-id")),
        CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
        SqlObject("value")
      )
    )

}
