// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ProposalReferenceView

trait ProposalReferenceMapping[F[_]]
  extends BaseMapping[F]
     with ProposalReferenceView[F] {

  lazy val ProposalReferenceMapping: ObjectMapping =
    ObjectMapping(
      tpe = ProposalReferenceType,
      fieldMappings = List(
        SqlField("id",            ProposalReferenceView.Id, key = true, hidden = true),
        SqlField("label",         ProposalReferenceView.ProposalReference),
        SqlField("semester",      ProposalReferenceView.Semester),
        SqlField("semesterIndex", ProposalReferenceView.SemesterIndex),

        // Used for WHERE clause matching
        SqlField("labelString",   ProposalReferenceView.ProposalReferenceString, hidden = true)
      )
    )

}
