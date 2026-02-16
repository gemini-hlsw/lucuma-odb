// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import cats.syntax.apply.*
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import io.circe.syntax.*
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.time.query.given
import lucuma.odb.service.Services

import table.AtomRecordView
import table.Flamingos2DynamicView
import table.GmosDynamicTables
import table.StepRecordView
import table.VisitTable

trait StepRecordMapping[F[_]] extends StepRecordView[F]
                                 with AtomRecordView[F]
                                 with EventRangeEffectHandler[F]
                                 with KeyValueEffectHandler[F]
                                 with Flamingos2DynamicView[F]
                                 with GmosDynamicTables[F]
                                 with Predicates[F]
                                 with SelectSubquery
                                 with VisitTable[F] {
  def user: User
  def services: Resource[F, Services[F]]

  lazy val StepRecordMapping: ObjectMapping =
    ObjectMapping(StepRecordType)(
      SqlField("id",              StepRecordView.Id, key = true),
      SqlField("index",           StepRecordView.StepIndex),
      SqlField("instrument",      StepRecordView.Instrument, discriminator = true),
      SqlObject("atom",           Join(StepRecordView.AtomId, AtomRecordView.Id)),
      SqlField("executionState",  StepRecordView.ExecutionState),
      SqlField("_firstEventTime", StepRecordView.FirstEventTime, hidden = true),
      SqlField("_lastEventTime",  StepRecordView.LastEventTime, hidden = true),
      CursorFieldJson("interval", c =>
        for
          f <- c.fieldAs[Option[Timestamp]]("_firstEventTime")
          l <- c.fieldAs[Option[Timestamp]]("_lastEventTime")
        yield (f, l).mapN((first, last) => TimestampInterval.between(first, last)).asJson,
        List("_firstEventTime", "_lastEventTime")
      ),
      SqlObject("stepConfig"),
      SqlObject("telescopeConfig"),
      SqlField("observeClass",    StepRecordView.ObserveClass),
      SqlObject("estimate"),
      SqlField("qaState",         StepRecordView.QaState),
      SqlObject("datasets"),
      SqlObject("events"),
      SqlObject("flamingos2",     Join(StepRecordView.Id, Flamingos2DynamicView.Id)),
      SqlObject("gmosNorth",      Join(StepRecordView.Id, GmosNorthDynamicTable.Id)),
      SqlObject("gmosSouth",      Join(StepRecordView.Id, GmosSouthDynamicTable.Id))
    )

  lazy val StepRecordElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (StepRecordType, "datasets", List(
      DatasetIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, DatasetType, "id", Predicates.dataset.id, Predicates.dataset.observation.program)

    case (StepRecordType, "events", List(
      ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, ExecutionEventType, "id", Predicates.executionEvent.id, Predicates.executionEvent.observation.program)

  }

}
