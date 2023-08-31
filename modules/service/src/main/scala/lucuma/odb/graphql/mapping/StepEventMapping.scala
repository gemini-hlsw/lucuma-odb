// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.ObservationView
import table.StepEventTable
import table.StepRecordTable
import table.VisitTable

trait StepEventMapping[F[_]] extends StepEventTable[F]
                                with ObservationView[F]
                                with AtomRecordTable[F]
                                with StepRecordTable[F]
                                with VisitTable[F] {

  lazy val StepEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = StepEventType,
      fieldMappings = List(
        SqlField("id",           StepEventTable.Id, key = true),
        SqlField("stepId",       StepEventTable.StepId),
        SqlField("sequenceType", StepEventTable.SequenceType),
        SqlField("stepStage",    StepEventTable.StepStage),
        SqlObject("observation", Join(StepEventTable.StepId, StepRecordTable.Id), Join(StepRecordTable.AtomId, AtomRecordTable.Id), Join(AtomRecordTable.ObservationId, ObservationView.Id)),
        SqlField("received",     StepEventTable.Received)
      )
    )

}