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
import table.ExecutionEventView
import table.ObservationView
import table.VisitTable

trait ExecutionEventMapping[F[_]] extends ExecutionEventView[F]
                                     with DatasetTable[F]
                                     with ObservationView[F]
                                     with VisitTable[F] {

  lazy val ExecutionEventMapping: ObjectMapping =
    SqlInterfaceMapping(
      tpe           = ExecutionEventType,
      discriminator = executionEventTypeDiscriminator,
      fieldMappings = List(
        SqlField("id",           ExecutionEventView.Id, key = true),
//        SqlObject("visit",       Join(ExecutionEventView.VisitId,       VisitTable.Id)),
        SqlField("visitId",      ExecutionEventView.VisitId),
        SqlObject("observation", Join(ExecutionEventView.ObservationId, ObservationView.Id)),
        SqlField("received",     ExecutionEventView.Received),
        SqlField("eventType",    ExecutionEventView.EventType, discriminator = true)
      )
    )

  private lazy val executionEventTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {
      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[lucuma.odb.data.ExecutionEventType]("eventType").map {
          case lucuma.odb.data.ExecutionEventType.Sequence => SequenceEventType
          case lucuma.odb.data.ExecutionEventType.Step     => StepEventType
          case lucuma.odb.data.ExecutionEventType.Dataset  => DatasetEventType
        }

      private def mkPredicate(tpe: lucuma.odb.data.ExecutionEventType): Option[Predicate] =
        Eql(ExecutionEventTypeType / "eventType", Const(tpe)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case SequenceEventType => mkPredicate(lucuma.odb.data.ExecutionEventType.Sequence)
          case StepEventType     => mkPredicate(lucuma.odb.data.ExecutionEventType.Step)
          case DatasetEventType  => mkPredicate(lucuma.odb.data.ExecutionEventType.Dataset)
          case _                 => none
        }
    }

  lazy val SequenceEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = SequenceEventType,
      fieldMappings = List(
        SqlField("id",      ExecutionEventView.Id, key = true),
        SqlField("command", ExecutionEventView.SequenceCommand)
      )
    )

  lazy val StepEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = StepEventType,
      fieldMappings = List(
        SqlField("id",        ExecutionEventView.Id, key = true),
        SqlField("stepId",    ExecutionEventView.StepId),
        SqlField("stepStage", ExecutionEventView.StepStage)
      )
    )

  lazy val DatasetEventMapping: ObjectMapping =
    ObjectMapping(
      tpe = DatasetEventType,
      fieldMappings = List(
        SqlField("id",           ExecutionEventView.Id, key = true),
        SqlField("datasetStage", ExecutionEventView.DatasetStage),
        SqlObject("dataset",     Join(ExecutionEventView.DatasetId, DatasetTable.Id))
      )
    )

}
