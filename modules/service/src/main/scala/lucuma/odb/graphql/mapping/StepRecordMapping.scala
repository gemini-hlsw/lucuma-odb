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

import table.AtomTable
import table.Flamingos2DynamicView
import table.GmosDynamicTables
import table.StepView
import table.VisitTable

trait StepRecordMapping[F[_]] extends StepView[F]
                                 with AtomTable[F]
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
      SqlField("id",              StepView.Id, key = true),
      SqlField("index",           StepView.StepIndex),
      SqlField("instrument",      StepView.Instrument, discriminator = true),
      SqlObject("atom",           Join(StepView.AtomId, AtomTable.Id)),
      SqlField("executionState",  StepView.ExecutionState),
      SqlField("_firstEventTime", StepView.FirstEvent, hidden = true),
      SqlField("_lastEventTime",  StepView.LastEvent, hidden = true),
      CursorFieldJson("interval", c =>
        for
          f <- c.fieldAs[Option[Timestamp]]("_firstEventTime")
          l <- c.fieldAs[Option[Timestamp]]("_lastEventTime")
        yield (f, l).mapN((first, last) => TimestampInterval.between(first, last)).asJson,
        List("_firstEventTime", "_lastEventTime")
      ),
      SqlObject("stepConfig"),
      SqlObject("telescopeConfig"),
      SqlField("observeClass",    StepView.ObserveClass),
      SqlObject("estimate"),
      SqlField("qaState",         StepView.QaState),
      SqlObject("datasets"),
      SqlObject("events"),
      SqlObject("flamingos2",     Join(StepView.Id, Flamingos2DynamicView.Id)),
      SqlObject("gmosNorth",      Join(StepView.Id, GmosNorthDynamicTable.Id)),
      SqlObject("gmosSouth",      Join(StepView.Id, GmosSouthDynamicTable.Id))
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
