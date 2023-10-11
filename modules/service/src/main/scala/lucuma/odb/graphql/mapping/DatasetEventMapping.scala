// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.DatasetTable
import table.DatasetEventTable
import table.ObservationView

trait DatasetEventMapping[F[_]] extends DatasetEventTable[F]
                                   with DatasetTable[F]
                                   with ObservationView[F] {

  lazy val DatasetEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetEventType,
      fieldMappings = List(
        SqlField("id",           DatasetEventTable.Id, key = true),
        SqlObject("dataset",     Join(DatasetEventTable.DatasetId, DatasetTable.Id)),
        SqlField("datasetStage", DatasetEventTable.DatasetStage),
        SqlObject("observation", Join(DatasetEventTable.DatasetId, DatasetTable.Id), Join(DatasetTable.ObservationId, ObservationView.Id)),
        SqlField("received",     DatasetEventTable.Received)
      )
    )

}
