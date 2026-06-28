// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import lucuma.core.model.Target
import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ProgramView
import lucuma.odb.graphql.table.TargetView


trait TargetEditMapping[F[_]] extends TargetView[F] with ProgramView[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val TargetEditMapping: ObjectMapping =
    ObjectMapping(TargetEditType)(
      SqlField("synthetic-id", ProgramView.Id, key = true, hidden = true),
      CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
      CursorField("targetId", _.envR[Target.Id]("targetId"), List("synthetic-id")),
      SqlObject("value", Join(ProgramView.Id, TargetView.ProgramId))
    )

}
