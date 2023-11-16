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

import table.DatasetTable
import table.ExecutionEventTable
import table.ObservationView
import table.StepRecordTable
import table.VisitTable

trait ExecutionEventMapping[F[_]] extends ExecutionEventTable[F]
                                     with DatasetTable[F]
                                     with ObservationView[F]
                                     with StepRecordTable[F]
                                     with VisitTable[F] {

  lazy val ExecutionEventMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = ExecutionEventType,
      discriminator = executionEventTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",           ExecutionEventTable.Id, key = true),
        SqlObject("visit",       Join(ExecutionEventTable.VisitId,       VisitTable.Id)),
        SqlObject("observation", Join(ExecutionEventTable.ObservationId, ObservationView.Id)),
        SqlField("received",     ExecutionEventTable.Received),
        SqlField("eventType",    ExecutionEventTable.EventType, discriminator = true)
      )
    )

  private lazy val executionEventTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {

      import lucuma.odb.data.ExecutionEventType.*

      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[lucuma.odb.data.ExecutionEventType]("eventType").map {
          case Sequence => SequenceEventType
          case Step     => StepEventType
          case Dataset  => DatasetEventType
        }

      private def mkPredicate(eventType: lucuma.odb.data.ExecutionEventType): Option[Predicate] =
        Eql(ExecutionEventType / "eventType", Const(eventType)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case SequenceEventType => mkPredicate(Sequence)
          case StepEventType     => mkPredicate(Step)
          case DatasetEventType  => mkPredicate(Dataset)
          case _                 => none
        }
    }

  lazy val SequenceEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = SequenceEventType,
      fieldMappings = List(
        SqlField("id",      ExecutionEventTable.Id, key = true),
        SqlField("command", ExecutionEventTable.SequenceCommand)
      )
    )

  lazy val StepEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = StepEventType,
      fieldMappings = List(
        SqlField("id",        ExecutionEventTable.Id, key = true),
        SqlObject("step",     Join(ExecutionEventTable.StepId, StepRecordTable.Id)),
        SqlField("stepStage", ExecutionEventTable.StepStage)
      )
    )

  lazy val DatasetEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetEventType,
      fieldMappings = List(
        SqlField("id",           ExecutionEventTable.Id, key = true),
        SqlObject("step",        Join(ExecutionEventTable.StepId, StepRecordTable.Id)),
        SqlObject("dataset",     Join(ExecutionEventTable.DatasetId, DatasetTable.Id)),
        SqlField("datasetStage", ExecutionEventTable.DatasetStage)
      )
    )

}
