// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import eu.timepit.refined.cats.given
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.QueryCompiler.Elab
import grackle.TypeRef
import io.circe.Encoder as CirceEncoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.binding.PosIntBinding
import lucuma.odb.graphql.predicate.Predicates
import lucuma.odb.json.time.query.given
import lucuma.odb.json.timeaccounting.given
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.TimeAccounting

import table.AtomRecordView
import table.ExecutionEventTable
import table.Flamingos2StaticTable
import table.GmosStaticTables
import table.GnirsStaticTable
import table.Igrins2StaticTable
import table.ObservationView
import table.VisitTable

trait VisitMapping[F[_]] extends VisitTable[F]
                            with AtomRecordView[F]
                            with EventRangeEffectHandler[F]
                            with KeyValueEffectHandler[F]
                            with ExecutionEventTable[F]
                            with Flamingos2StaticTable[F]
                            with GmosStaticTables[F]
                            with GnirsStaticTable[F]
                            with Igrins2StaticTable[F]
                            with ObservationView[F]
                            with Predicates[F]
                            with SelectSubquery {

  def services(using User): Resource[F, Services[F]]

  lazy val VisitMapping: ObjectMapping =
    ObjectMapping(VisitType)(
      SqlField("id",            VisitTable.Id,         key = true),
      SqlField("instrument",    VisitTable.Instrument, discriminator = true),
      SqlObject("observation",  Join(VisitTable.ObservationId, ObservationView.Id)),
      SqlField("recordedTime",  VisitTable.RecordedTime),
      SqlField("created",       VisitTable.RecordedTime),
      SqlField("clientTime",    VisitTable.ClientTime),
      SqlField("effectiveTime", VisitTable.EffectiveTime),
      SqlField("site",          VisitTable.Site),
      EffectField("interval", intervalHandler, List("id")),
      SqlObject("atomRecords"),
      SqlObject("datasets"),
      SqlObject("events"),
      SqlObject("timeChargeInvoice"),
      EffectField("setups", setupsHandler, List("id")),
      SqlField("idempotencyKey", VisitTable.IdempotencyKey),
      SqlObject("flamingos2", Join(VisitTable.ObservationId, Flamingos2StaticTable.ObservationId)),
      SqlObject("gmosNorth",  Join(VisitTable.ObservationId, GmosNorthStaticTable.ObservationId)),
      SqlObject("gmosSouth",  Join(VisitTable.ObservationId, GmosSouthStaticTable.ObservationId)),
      SqlObject("igrins2",    Join(VisitTable.ObservationId, Igrins2StaticTable.ObservationId)),
      SqlObject("gnirs",      Join(VisitTable.ObservationId, GnirsStaticTable.ObservationId))
    )

  lazy val VisitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (VisitType, "atomRecords", List(
      PosIntBinding.Option("OFFSET", rOFFSET),
      PosIntBinding.Option("OFFSET_AT", rOFFSET_AT),
      PosIntBinding.Option("OFFSET_AFTER", rOFFSET_AFTER),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rOFFSET_AT, rOFFSET_AFTER, rLIMIT, AtomRecordType, "index", Predicates.atomRecord.index, Predicates.atomRecord.observation.program)

    case (VisitType, "datasets", List(
      DatasetIdBinding.Option("OFFSET", rOFFSET),
      DatasetIdBinding.Option("OFFSET_AT", rOFFSET_AT),
      DatasetIdBinding.Option("OFFSET_AFTER", rOFFSET_AFTER),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rOFFSET_AT, rOFFSET_AFTER, rLIMIT, DatasetType, "id", Predicates.dataset.id, Predicates.dataset.observation.program)

    case (VisitType, "events", List(
      ExecutionEventIdBinding.Option("OFFSET", rOFFSET),
      ExecutionEventIdBinding.Option("OFFSET_AT", rOFFSET_AT),
      ExecutionEventIdBinding.Option("OFFSET_AFTER", rOFFSET_AFTER),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rOFFSET_AT, rOFFSET_AFTER, rLIMIT, ExecutionEventType, "id", Predicates.executionEvent.id, Predicates.executionEvent.observation.program)
  }

  private given CirceEncoder[TimeAccounting.Setup] =
    CirceEncoder.instance: s =>
      Json.obj(
        "kind"       -> s.kind.asJson,
        "interval"   -> s.interval.asJson,
        "timeCharge" -> s.charge.asJson
      )

  private lazy val setupsHandler: EffectHandler[F] =
    keyValueEffectHandler[Visit.Id, List[TimeAccounting.Setup]]("id"): vid =>
      services.useTransactionally:
        Services.asSuperUser:
          timeAccountingService.selectVisitSetups(vid)

  private lazy val intervalHandler: EffectHandler[F] =
    eventRangeEffectHandler[Visit.Id]("id", services, executionEventService.visitRange)

}
