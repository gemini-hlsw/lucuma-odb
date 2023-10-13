// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.DatasetEventTable
import table.ObservationView
import table.StepRecordTable

trait DatasetEventMapping[F[_]] extends DatasetEventTable[F]
                                   with ObservationView[F]
                                   with AtomRecordTable[F]
                                   with StepRecordTable[F] {

  lazy val DatasetEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetEventType,
      fieldMappings = List(
        SqlField("id",           DatasetEventTable.Id, key = true),
        SqlObject("datasetId"),
        SqlField("datasetStage", DatasetEventTable.DatasetStage),
        SqlObject("observation", Join(DatasetEventTable.DatasetId.StepId, StepRecordTable.Id), Join(StepRecordTable.AtomId, AtomRecordTable.Id), Join(AtomRecordTable.ObservationId, ObservationView.Id)),
        SqlField("received",     DatasetEventTable.Received)
      )
    )


}
