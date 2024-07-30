// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import grackle.Result
import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable


trait ObservationEditMapping[F[_]] extends ObservationView[F] with ProgramTable[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val ObservationEditMapping: ObjectMapping =
    ObjectMapping(ObservationEditType)(
      SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
      CursorField("id", _ => Result(0L), List("synthetic-id")),
      CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
      SqlObject("value", Join(ProgramTable.Id, ObservationView.ProgramId))
    )

}
