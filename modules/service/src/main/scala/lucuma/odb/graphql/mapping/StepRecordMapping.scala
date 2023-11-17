// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import cats.syntax.option.*
import grackle.Cursor
import grackle.Predicate
import grackle.Predicate.Const
import grackle.Predicate.Eql
import grackle.Query.Binding
import grackle.QueryCompiler.Elab
import grackle.Result
import grackle.Type
import grackle.TypeRef
import lucuma.core.enums.Instrument
import lucuma.core.model.User
import lucuma.odb.graphql.binding.DatasetIdBinding
import lucuma.odb.graphql.binding.ExecutionEventIdBinding
import lucuma.odb.graphql.binding.NonNegIntBinding
import lucuma.odb.graphql.predicate.Predicates

import table.AtomRecordTable
import table.GmosDynamicTables
import table.StepRecordView
import table.VisitTable

trait StepRecordMapping[F[_]] extends StepRecordView[F]
                                 with AtomRecordTable[F]
                                 with GmosDynamicTables[F]
                                 with Predicates[F]
                                 with SelectSubquery
                                 with VisitTable[F] {
  def user: User

  lazy val StepRecordMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = StepRecordType,
      discriminator = stepRecordTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",           StepRecordView.Id, key = true),
        SqlField("instrument",   StepRecordView.Instrument, discriminator = true),
        SqlObject("atom",        Join(StepRecordView.AtomId, AtomRecordTable.Id)),
        SqlField("created",      StepRecordView.Created),
        SqlObject("stepConfig"),
        SqlField("observeClass", StepRecordView.ObserveClass),

        // TBD: startTime, endTime, duration, stepQaState

        SqlObject("datasets"),
        SqlObject("events")
      )
    )

  private lazy val stepRecordTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[Instrument]("instrument").flatMap {
          case Instrument.GmosNorth => Result(GmosNorthStepRecordType)
          case Instrument.GmosSouth => Result(GmosSouthStepRecordType)
          case inst                 => Result.failure(s"No StepRecord implementation for ${inst.shortName}")
        }

      private def mkPredicate(instrument: Instrument): Option[Predicate] =
        Eql(StepRecordType / "instrument", Const(instrument)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case GmosNorthStepRecordType => mkPredicate(Instrument.GmosNorth)
          case GmosSouthStepRecordType => mkPredicate(Instrument.GmosSouth)
          case _                       => none
        }
    }

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

  lazy val GmosNorthStepRecordMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosNorthStepRecordType,
      fieldMappings = List(
        SqlField("id", StepRecordView.Id, key = true),
        SqlObject("instrumentConfig", Join(StepRecordView.Id, GmosNorthDynamicTable.Id))
      )
    )

  lazy val GmosSouthStepRecordMapping: ObjectMapping =
    ObjectMapping(
      tpe = GmosSouthStepRecordType,
      fieldMappings = List(
        SqlField("id", StepRecordView.Id, key = true),
        SqlObject("instrumentConfig", Join(StepRecordView.Id, GmosSouthDynamicTable.Id))
      )
    )

}