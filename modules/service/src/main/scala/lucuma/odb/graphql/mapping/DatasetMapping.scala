// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.parallel.*
import grackle.Predicate.True
import grackle.Predicate.and
import grackle.Query.Binding
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
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
                              with StepRecordTable[F]
                              with VisitTable[F] {
  def user: User

  lazy val DatasetMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetType,
      fieldMappings = List(
        SqlField("id",     DatasetTable.Id,   key = true),
        SqlField("stepId", DatasetTable.StepId),
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
      Elab.transformChild { child =>
        (rOFFSET, rLIMIT).parTupled.flatMap { (OFFSET, LIMIT) =>
          val limit = LIMIT.foldLeft(ResultMapping.MaxLimit)(_ min _.value)
          ResultMapping.selectResult(child, limit) { q =>
            FilterOrderByOffsetLimit(
              pred = Some(and(List(
                OFFSET.map(Predicates.executionEvent.id.gtEql).getOrElse(True),
                Predicates.executionEvent.observation.program.isVisibleTo(user),
              ))),
              oss = Some(List(OrderSelection[ExecutionEvent.Id](ExecutionEventType / "id"))),
              offset = None,
              limit = Some(limit + 1), // Select one extra row here.
              child = q
            )
          }
        }
      }
  }


}
