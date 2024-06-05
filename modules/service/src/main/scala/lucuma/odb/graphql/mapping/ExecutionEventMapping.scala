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

import table.AtomRecordTable
import table.DatasetTable
import table.ExecutionEventTable
import table.ObservationView
import table.StepRecordView
import table.VisitTable

trait ExecutionEventMapping[F[_]] extends ExecutionEventTable[F]
                                     with AtomRecordTable[F]
                                     with DatasetTable[F]
                                     with ObservationView[F]
                                     with StepRecordView[F]
                                     with VisitTable[F] {

  lazy val ExecutionEventMapping: ObjectMapping =
    SqlInterfaceMapping(ExecutionEventType, executionEventTypeDiscriminator)(
      SqlField("id",           ExecutionEventTable.Id, key = true, hidden = true),
      SqlObject("visit",       Join(ExecutionEventTable.VisitId,       VisitTable.Id)),
      SqlObject("observation", Join(ExecutionEventTable.ObservationId, ObservationView.Id)),
      SqlField("received",     ExecutionEventTable.Received),
      SqlField("eventType",    ExecutionEventTable.EventType, discriminator = true),

      // Hidden fields used in the WhereExecutionEvent predicate.  There
      // appears to be no good way to create a predicate that matches on a
      // particular interface implementation so this is the best we can do.
      // We can match on fields that appear in the ExecutionEventTable.
      SqlField("_atomId",          ExecutionEventTable.AtomId,          hidden = true),
      SqlField("_sequenceCommand", ExecutionEventTable.SequenceCommand, hidden = true),
      SqlField("_slewStage",       ExecutionEventTable.SlewStage,       hidden = true),
      SqlField("_stepId",          ExecutionEventTable.StepId,          hidden = true),
      SqlField("_stepStage",       ExecutionEventTable.StepStage,       hidden = true),
      SqlField("_datasetId",       ExecutionEventTable.DatasetId,       hidden = true),
      SqlField("_datasetStage",    ExecutionEventTable.DatasetStage,    hidden = true)
    )

  private lazy val executionEventTypeDiscriminator: SqlDiscriminator =
    new SqlDiscriminator {

      import lucuma.odb.data.ExecutionEventType.*

      override def discriminate(c: Cursor): Result[Type] =
        c.fieldAs[lucuma.odb.data.ExecutionEventType]("eventType").map {
          case Sequence => SequenceEventType
          case Slew     => SlewEventType
          case Atom     => AtomEventType
          case Step     => StepEventType
          case Dataset  => DatasetEventType
        }

      private def mkPredicate(eventType: lucuma.odb.data.ExecutionEventType): Option[Predicate] =
        Eql(ExecutionEventType / "eventType", Const(eventType)).some

      override def narrowPredicate(tpe: Type): Option[Predicate] =
        tpe match {
          case SequenceEventType => mkPredicate(Sequence)
          case SlewEventType     => mkPredicate(Slew)
          case AtomEventType     => mkPredicate(Atom)
          case StepEventType     => mkPredicate(Step)
          case DatasetEventType  => mkPredicate(Dataset)
          case _                 => none
        }
    }

  lazy val SequenceEventMapping: ObjectMapping =
    ObjectMapping(SequenceEventType)(
      SqlField("command", ExecutionEventTable.SequenceCommand)
    )

  lazy val SlewEventMapping: ObjectMapping =
    ObjectMapping(SlewEventType)(
      SqlField("slewStage", ExecutionEventTable.SlewStage)
    )

  lazy val AtomEventMapping: ObjectMapping =
    ObjectMapping(AtomEventType)(
      SqlObject("atom",     Join(ExecutionEventTable.AtomId, AtomRecordTable.Id)),
      SqlField("atomStage", ExecutionEventTable.AtomStage)
    )

  lazy val StepEventMapping: ObjectMapping =
  ObjectMapping(StepEventType)(
      SqlObject("atom",     Join(ExecutionEventTable.AtomId, AtomRecordTable.Id)),
      SqlObject("step",     Join(ExecutionEventTable.StepId, StepRecordView.Id)),
      SqlField("stepStage", ExecutionEventTable.StepStage)
    )

  lazy val DatasetEventMapping: ObjectMapping =
    ObjectMapping(DatasetEventType)(
      SqlObject("atom",        Join(ExecutionEventTable.AtomId, AtomRecordTable.Id)),
      SqlObject("step",        Join(ExecutionEventTable.StepId, StepRecordView.Id)),
      SqlObject("dataset",     Join(ExecutionEventTable.DatasetId, DatasetTable.Id)),
      SqlField("datasetStage", ExecutionEventTable.DatasetStage)
    )

}
