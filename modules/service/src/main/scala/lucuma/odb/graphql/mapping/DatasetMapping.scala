// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.apply.*
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import io.circe.syntax.*
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.time.query.given

import table.AtomRecordTable
import table.DatasetReferenceView
import table.DatasetTable
import table.ObservationView
import table.StepRecordView
import table.VisitTable

trait DatasetMapping[F[_]] extends DatasetTable[F]
                              with DatasetReferenceView[F]
                              with AtomRecordTable[F]
                              with ObservationView[F]
                              with Predicates[F]
                              with SelectSubquery
                              with StepRecordView[F]
                              with VisitTable[F] {
  def user: User

  lazy val DatasetMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetType,
      fieldMappings = List(
        SqlField("id",           DatasetTable.Id,   key = true),
        SqlObject("step",        Join(DatasetTable.StepId, StepRecordView.Id)),
        SqlField("index",        DatasetTable.ExposureIndex),
        SqlObject("reference",   Join(DatasetTable.Id, DatasetReferenceView.Id)),
        SqlObject("observation", Join(DatasetTable.ObservationId, ObservationView.Id)),
        SqlObject("visit",       Join(DatasetTable.StepId, StepRecordView.Id), Join(StepRecordView.AtomId, AtomRecordTable.Id), Join(AtomRecordTable.VisitId, VisitTable.Id)),
        SqlObject("events"),
        SqlField("filename",     DatasetTable.File.Name),
        SqlField("qaState",      DatasetTable.QaState),

        SqlField("start",        DatasetTable.Time.Start, hidden = true),
        SqlField("end",          DatasetTable.Time.End, hidden = true),

        CursorFieldJson("interval",
           cursor =>
             for {
               s <- cursor.fieldAs[Option[Timestamp]]("start")
               e <- cursor.fieldAs[Option[Timestamp]]("end")
             } yield (s, e).mapN { (ts, te) => TimestampInterval.between(ts, te) }.asJson,
           List("start", "end")
        )
      )
    )

  lazy val DatasetElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {
    case (DatasetType, "events", List(
      ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, ExecutionEventType, "id", Predicates.executionEvent.id, Predicates.executionEvent.observation.program)
  }


}
