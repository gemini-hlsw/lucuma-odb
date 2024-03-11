// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ObservationReferenceView
import table.ProgramReferenceView


trait ObservationReferenceMapping[F[_]]
  extends BaseMapping[F]
     with ObservationReferenceView[F]
     with ProgramReferenceView[F] {

  lazy val ObservationReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = ObservationReferenceType,
      fieldMappings = List(
        SqlField("id",       ObservationReferenceView.Id, key = true, hidden = true),
        SqlObject("program", Join(ObservationReferenceView.ProgramId, ProgramReferenceView.Id)),
        SqlField("index",    ObservationReferenceView.ObservationIndex),
        SqlField("label",    ObservationReferenceView.ObservationReference),

        // Used for WHERE clause matching
        SqlField("labelString", ObservationReferenceView.ObservationReferenceString, hidden = true)
      )
    )
}
