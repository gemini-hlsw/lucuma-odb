// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

//import table.AtomRecordTablep
import table.DatasetTable
import table.ObservationView
//import table.StepRecordTable

trait DatasetMapping[F[_]] extends DatasetTable[F]
                              with ObservationView[F] {
//                              with AtomRecordTable[F]
//                              with StepRecordTable[F] {

  lazy val DatasetMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetType,
      fieldMappings = List(
        SqlField("id",     DatasetTable.Id,   key = true),
        SqlField("stepId", DatasetTable.StepId),
        SqlField("index",  DatasetTable.Index),

        SqlObject("observation", Join(DatasetTable.ObservationId, ObservationView.Id)),
        SqlField("filename", DatasetTable.File.Name),
        SqlField("qaState",  DatasetTable.QaState),

        SqlField("start", DatasetTable.Time.Start),
        SqlField("end", DatasetTable.Time.End)
      )
    )


}
