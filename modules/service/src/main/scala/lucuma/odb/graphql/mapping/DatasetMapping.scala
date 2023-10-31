// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.predicate.Predicates

import table.AtomRecordTable
import table.DatasetTable
import table.ObservationView
import table.StepRecordTable
import table.VisitTable

trait DatasetMapping[F[_]] extends DatasetTable[F]
                              with AtomRecordTable[F]
                              with ObservationView[F]
                              with Predicates[F]
                              with SelectSubquery
                              with StepRecordTable[F]
                              with VisitTable[F] {
  def user: User

  lazy val DatasetMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetType,
      fieldMappings = List(
        SqlField("id",     DatasetTable.Id,   key = true),
        SqlObject("step",  Join(DatasetTable.StepId, StepRecordTable.Id)),
        SqlField("index",  DatasetTable.Index),

        SqlObject("observation", Join(DatasetTable.ObservationId, ObservationView.Id)),
        SqlObject("visit", Join(DatasetTable.StepId, StepRecordTable.Id), Join(StepRecordTable.AtomId, AtomRecordTable.Id), Join(AtomRecordTable.VisitId, VisitTable.Id)),
        SqlObject("events"),
        SqlField("filename", DatasetTable.File.Name),
        SqlField("qaState",  DatasetTable.QaState),

        SqlField("start", DatasetTable.Time.Start),
        SqlField("end", DatasetTable.Time.End)
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
