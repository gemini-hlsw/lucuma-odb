// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.option.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Result
import grackle.Type
import grackle.TypeRef
import lucuma.core.enums.Instrument

import table.ExecutionEventView
import table.GmosStaticTables
import table.VisitTable

trait VisitMapping[F[_]] extends VisitTable[F]
                            with ExecutionEventView[F]
                            with GmosStaticTables[F] {

  /*
    "Visit id."
  id: VisitId!

  "Implements the Visit.instrument field to return GMOS_SOUTH."
  instrument: Instrument!

  "Created by Observe at time."
  created: Timestamp!

  "Started at time."
  startTime: Timestamp

  "Ended at time."
  endTime: Timestamp

  "Visit duration."
  duration: TimeSpan!

  "Execution events associated with this visit."
  events: [ExecutionEvent!]!
     */

  lazy val VisitMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = VisitType,
      discriminator = visitTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",         VisitTable.Id,         key = true),
        SqlField("instrument", VisitTable.Instrument, discriminator = true),
        SqlField("created",    VisitTable.Created),
        SqlObject("events",    Join(VisitTable.Id, ExecutionEventView.VisitId))
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

/*
  """
  GmosSouth static instrument configuration
  """
  static: GmosSouthStatic!

  """
  GmosSouth recorded steps
  """
  steps: [GmosSouthStepRecord!]!
 */
}
