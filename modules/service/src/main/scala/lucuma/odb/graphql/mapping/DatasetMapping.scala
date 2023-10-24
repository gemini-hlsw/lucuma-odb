// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import table.AtomRecordTable
import table.DatasetTable
import table.ObservationView
import table.StepRecordTable
import table.VisitTable

trait DatasetMapping[F[_]] extends DatasetTable[F]
                              with AtomRecordTable[F]
                              with ObservationView[F]
                              with StepRecordTable[F]
                              with VisitTable[F] {

  lazy val DatasetMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetType,
      fieldMappings = List(
        SqlField("id",     DatasetTable.Id,   key = true),
        SqlField("stepId", DatasetTable.StepId),
        SqlField("index",  DatasetTable.Index),

        SqlObject("observation", Join(DatasetTable.ObservationId, ObservationView.Id)),
        SqlObject("visit", Join(DatasetTable.StepId, StepRecordTable.Id), Join(StepRecordTable.AtomId, AtomRecordTable.Id), Join(AtomRecordTable.VisitId, VisitTable.Id)),
        SqlField("filename", DatasetTable.File.Name),
        SqlField("qaState",  DatasetTable.QaState),

        SqlField("start", DatasetTable.Time.Start),
        SqlField("end", DatasetTable.Time.End)
      )
    )


}
