// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.ObservationView
import table.StepEventTable
import table.VisitTable

trait StepEventMapping[F[_]] extends StepEventTable[F]
                                with ObservationView[F]
                                with VisitTable[F] {

  lazy val StepEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = StepEventType,
      fieldMappings = List(
        SqlField("id",           StepEventTable.Id, key = true),
        SqlField("stepId",       StepEventTable.StepId),
        SqlField("visitId",      StepEventTable.VisitId),
        SqlField("sequenceType", StepEventTable.SequenceType),
        SqlField("stepStage",    StepEventTable.StepStage),
        SqlObject("observation", Join(StepEventTable.VisitId, VisitTable.Id), Join(VisitTable.ObservationId, ObservationView.Id)),
        SqlField("received",     StepEventTable.Received)
      )
    )

}