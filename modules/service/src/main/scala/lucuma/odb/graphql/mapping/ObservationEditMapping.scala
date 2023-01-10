// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping


import lucuma.odb.data.EditType
import lucuma.odb.graphql.table.ObservationView


trait ObservationEditMapping[F[_]] extends ObservationView[F] {

  // N.B. env is populated by the subscription elaborator
  lazy val ObservationEditMapping: ObjectMapping =
    ObjectMapping(
      tpe = ObservationEditType,
      fieldMappings = List(
        SqlField("synthetic-id", ObservationView.Id, key = true, hidden = true),
        CursorField("id", _.envR[Long]("id")),
        CursorField("editType", _.envR[EditType]("editType")),
        SqlObject("value")
      )
    )

}
