// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.TimestampBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*

import table.ExecutionEventTable
import table.Flamingos2StaticTable
import table.GmosStaticTables
import table.ObservationView
import table.VisitTable

trait VisitMapping[F[_]] extends VisitTable[F]
                            with EventRangeEffectHandler[F]
                            with ExecutionEventTable[F]
                            with Flamingos2StaticTable[F]
                            with GmosStaticTables[F]
                            with ObservationView[F]
                            with Predicates[F]
                            with SelectSubquery {

  def user: User
  def services: Resource[F, Services[F]]

  lazy val VisitMapping: ObjectMapping =
    ObjectMapping(VisitType)(
      SqlField("id",           VisitTable.Id,         key = true),
      SqlField("instrument",   VisitTable.Instrument, discriminator = true),
      SqlObject("observation", Join(VisitTable.ObservationId, ObservationView.Id)),
      SqlField("created",      VisitTable.Created),
      SqlField("site",         VisitTable.Site),
      EffectField("interval", intervalHandler, List("id")),
      SqlObject("atomRecords"),
      SqlObject("datasets"),
      SqlObject("events"),
      SqlObject("timeChargeInvoice"),
      SqlField("idempotencyKey", VisitTable.IdempotencyKey),
      SqlObject("flamingos2", Join(VisitTable.Id, Flamingos2StaticTable.VisitId)),
      SqlObject("gmosNorth",  Join(VisitTable.Id, GmosNorthStaticTable.VisitId)),
      SqlObject("gmosSouth",  Join(VisitTable.Id, GmosSouthStaticTable.VisitId))
    )

  lazy val VisitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (VisitType, "atomRecords", List(
      TimestampBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, AtomRecordType, "_firstEventTime", Predicates.atomRecord.firstEventTime, Predicates.atomRecord.visit.observation.program)

    case (VisitType, "datasets", List(
      DatasetIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, DatasetType, "id", Predicates.dataset.id, Predicates.dataset.observation.program)

    case (VisitType, "events", List(
      ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, ExecutionEventType, "id", Predicates.executionEvent.id, Predicates.executionEvent.observation.program)
  }

  private lazy val intervalHandler: EffectHandler[F] =
    eventRangeEffectHandler[Visit.Id]("id", services, executionEventService.visitRange)

}
