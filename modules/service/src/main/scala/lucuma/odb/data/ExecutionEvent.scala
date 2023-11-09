// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.eq.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp

sealed trait ExecutionEvent extends Product with Serializable {

  import ExecutionEvent.*

  def eventType:     ExecutionEventType
  def received:      Timestamp
  def observationId: Observation.Id
  def visitId:       Visit.Id

  def fold[A](
    sequenceEvent: SequenceEvent => A,
    stepEvent:     StepEvent     => A,
    datasetEvent:  DatasetEvent  => A
  ): A =
    this match {
      case e@SequenceEvent(_, _, _, _)      => sequenceEvent(e)
      case e@StepEvent(_, _, _, _, _)       => stepEvent(e)
      case e@DatasetEvent(_, _, _, _, _, _) => datasetEvent(e)
    }

}

object ExecutionEvent {

  case class SequenceEvent(
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    command:       SequenceCommand
  ) extends ExecutionEvent {

    override def eventType: ExecutionEventType =
      ExecutionEventType.Sequence

  }

  object SequenceEvent {

    given Eq[SequenceEvent] =
      Eq.by { a => (
        a.received,
        a.observationId,
        a.visitId,
        a.command
      )}

  }

  case class StepEvent(
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    stage:         StepStage
  ) extends ExecutionEvent {

    override def eventType: ExecutionEventType =
      ExecutionEventType.Step

  }

  object StepEvent {

    given Eq[StepEvent] =
      Eq.by { a => (
        a.received,
        a.observationId,
        a.visitId,
        a.stepId,
        a.stage
      )}

  }

  case class DatasetEvent(
    received:      Timestamp,
    observationId: Observation.Id,
    visitId:       Visit.Id,
    stepId:        Step.Id,
    datasetId:     Dataset.Id,
    stage:         DatasetStage
  ) extends ExecutionEvent {

    override def eventType: ExecutionEventType =
      ExecutionEventType.Dataset

  }

  object DatasetEvent {

    given Eq[DatasetEvent] =
      Eq.by { a => (
        a.received,
        a.observationId,
        a.visitId,
        a.stepId,
        a.datasetId,
        a.stage
      )}

  }


  given Eq[ExecutionEvent] =
    Eq.instance {
      case (e0: SequenceEvent, e1: SequenceEvent) => e0 === e1
      case (e0: StepEvent, e1: StepEvent)         => e0 === e1
      case (e0: DatasetEvent, e1: DatasetEvent)   => e0 === e1
      case _ => false
    }

}
