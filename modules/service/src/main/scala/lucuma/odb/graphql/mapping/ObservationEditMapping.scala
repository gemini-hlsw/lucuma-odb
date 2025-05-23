// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import lucuma.core.model.Observation
import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ObservationView
import lucuma.odb.graphql.table.ProgramTable


trait ObservationEditMapping[F[_]] extends ObservationView[F] with ProgramTable[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val ObservationEditMapping: ObjectMapping =
    ObjectMapping(ObservationEditType)(
      SqlField("synthetic-id", ProgramTable.Id, key = true, hidden = true),
      CursorField("editType", _.envR[EditType]("editType"), List("synthetic-id")),
      CursorField("observationId", _.envR[Observation.Id]("observationId"), List("synthetic-id")),
      SqlObject("value", Join(ProgramTable.Id, ObservationView.ProgramId))
    )

}
