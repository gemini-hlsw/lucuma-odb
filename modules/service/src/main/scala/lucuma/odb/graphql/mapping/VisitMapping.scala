// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.effect.Resource
import cats.syntax.option.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Query
import grackle.Query.Binding
import grackle.Query.EffectHandler
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.Type
import grackle.TypeRef
import lucuma.core.enums.Instrument
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
import table.GmosStaticTables
import table.ObservationView
import table.VisitTable

trait VisitMapping[F[_]] extends VisitTable[F]
                            with EventRangeEffectHandler[F]
                            with ExecutionEventTable[F]
                            with GmosStaticTables[F]
                            with ObservationView[F]
                            with Predicates[F]
                            with SelectSubquery {

  def user: User
  def services: Resource[F, Services[F]]

  lazy val VisitMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = VisitType,
      discriminator = visitTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",           VisitTable.Id,         key = true),
        SqlField("instrument",   VisitTable.Instrument, discriminator = true),
        SqlObject("observation", Join(VisitTable.ObservationId, ObservationView.Id)),
        SqlField("created",      VisitTable.Created),
        EffectField("interval", intervalHandler, List("id")),
        SqlObject("atomRecords"),
        SqlObject("datasets"),
        SqlObject("events"),
        SqlObject("timeChargeInvoice")
      )
    )

  private lazy val visitTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[Instrument]("instrument").flatMap {
          case Instrument.GmosNorth => Result(GmosNorthVisitType)
          case Instrument.GmosSouth => Result(GmosSouthVisitType)
          case inst                 => Result.internalError(s"No Visit implementation for ${inst.shortName}")
        }

      private def mkPredicate(instrument: Instrument): Option[Predicate] =
        Eql(VisitType / "instrument", Const(instrument)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case GmosNorthVisitType => mkPredicate(Instrument.GmosNorth)
          case GmosSouthVisitType => mkPredicate(Instrument.GmosSouth)
          case _                  => none
        }
    }

  lazy val GmosNorthVisitMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosNorthVisitType,
      fieldMappings = List(
        SqlField("id",      VisitTable.Id, key = true),
        SqlObject("static", Join(VisitTable.Id, GmosNorthStaticTable.VisitId))
      )
    )

  lazy val GmosSouthVisitMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosSouthVisitType,
      fieldMappings = List(
        SqlField("id",      VisitTable.Id, key = true),
        SqlObject("static", Join(VisitTable.Id, GmosSouthStaticTable.VisitId))
      )
    )

  lazy val VisitElaborator: PartialFunction[(TypeRef, String, List[Binding]), Elab[Unit]] = {

    case (VisitType, "atomRecords", List(
      TimestampBinding.Option("OFFSET", rOFFSET),
      NonNegIntBinding.Option("LIMIT", rLIMIT)
    )) =>
      selectWithOffsetAndLimit(rOFFSET, rLIMIT, AtomRecordType, "created", Predicates.atomRecord.created, Predicates.atomRecord.visit.observation.program)

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
