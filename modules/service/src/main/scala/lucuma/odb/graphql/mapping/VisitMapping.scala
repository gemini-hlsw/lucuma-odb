// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Predicate.True
import grackle.Predicate.and
import grackle.Query.Binding
import grackle.Query.FilterOrderByOffsetLimit
import grackle.Query.OrderSelection
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.Type
import grackle.TypeRef
import lucuma.core.enums.Instrument
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.User
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.predicate.Predicates

import table.ExecutionEventView
import table.GmosStaticTables
import table.ObservationView
import table.VisitTable

trait VisitMapping[F[_]] extends VisitTable[F]
                            with ExecutionEventView[F]
                            with GmosStaticTables[F]
                            with ObservationView[F]
                            with Predicates[F] {

  def user: User

  /*
  "Started at time."
  startTime: Timestamp

  "Ended at time."
  endTime: Timestamp

  "Visit duration."
  duration: TimeSpan!
  */

  lazy val VisitMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = VisitType,
      discriminator = visitTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",           VisitTable.Id,         key = true),
        SqlField("instrument",   VisitTable.Instrument, discriminator = true),
        SqlObject("observation", Join(VisitTable.ObservationId, ObservationView.Id)),
        SqlField("created",      VisitTable.Created),
        SqlObject("events")
      )
    )

  private lazy val visitTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[Instrument]("instrument").flatMap {
          case Instrument.GmosNorth => Result(GmosNorthVisitType)
          case Instrument.GmosSouth => Result(GmosSouthVisitType)
          case inst                 => Result.failure(s"No Visit implementation for ${inst.shortName}")
        }

      private def mkPredicate(tpe: Instrument): Option[Predicate] =
        Eql(InstrumentType / "instrument", Const(tpe)).some

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
    case (VisitType, "events", List(
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
