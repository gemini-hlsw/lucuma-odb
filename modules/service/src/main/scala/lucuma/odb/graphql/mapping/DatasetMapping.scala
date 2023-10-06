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
        SqlField("stepId", DatasetTable.DatasetId.StepId, hidden = true, key = true),
        SqlField("index",  DatasetTable.DatasetId.Index,  hidden = true, key = true),
//        CursorFieldJson("id", cursor => {
//          for {
//            s <- cursor.fieldAs[Step.Id]("stepId")
//            i <- cursor.fieldAs[Short]("index").map(PosShort.unsafeFrom)
//          } yield Dataset.Id(s, i).asJson
//        }, List("stepId", "index")),
        SqlObject("id"),

//        SqlObject("observation", Join(DatasetTable.DatasetId.StepId, StepRecordTable.Id), Join(StepRecordTable.AtomId, AtomRecordTable.Id), Join(AtomRecordTable.ObservationId, ObservationView.Id)),
        SqlObject("observation", Join(DatasetTable.ObservationId, ObservationView.Id)),
        SqlField("filename", DatasetTable.File.Name),
        SqlField("qaState",  DatasetTable.QaState),

        SqlField("start", DatasetTable.Time.Start),
        SqlField("end", DatasetTable.Time.End)
      )
    )


}
