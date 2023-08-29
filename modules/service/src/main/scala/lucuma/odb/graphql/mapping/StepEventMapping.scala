// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomTable
import table.ObservationView
import table.StepEventTable
import table.StepTable
import table.VisitTable

trait StepEventMapping[F[_]] extends StepEventTable[F]
                                with ObservationView[F]
                                with AtomTable[F]
                                with StepTable[F]
                                with VisitTable[F] {

  lazy val StepEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = StepEventType,
      fieldMappings = List(
        SqlField("id",           StepEventTable.Id, key = true),
        SqlField("stepId",       StepEventTable.StepId),
        SqlField("sequenceType", StepEventTable.SequenceType),
        SqlField("stepStage",    StepEventTable.StepStage),
        SqlObject("observation", Join(StepEventTable.StepId, StepTable.Id), Join(StepTable.AtomId, AtomTable.Id), Join(AtomTable.ObservationId, ObservationView.Id)),
        SqlField("received",     StepEventTable.Received)
      )
    )

}