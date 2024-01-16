// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.StepStage
import lucuma.core.model.ExecutionEvent
import lucuma.core.model.Observation
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.Step
import lucuma.core.util.Timestamp
import lucuma.odb.data.ExecutionEventType
import lucuma.odb.graphql.binding.*

object WhereExecutionEvent {

  def binding(path: Path): Matcher[Predicate] = {
    val WhereExecutionEventIdBinding   = WhereOrder.binding[ExecutionEvent.Id](path / "id", ExecutionEventIdBinding)
    val WhereVisitIdBinding            = WhereEq.binding[Visit.Id](path / "visit" / "id", VisitIdBinding)
    val WhereObservationIdBinding      = WhereOrder.binding[Observation.Id](path / "observation" / "id", ObservationIdBinding)
    val WhereTimestampBinding          = WhereOrder.binding[Timestamp](path / "received", TimestampBinding)
    val WhereExecutionEventTypeBinding = WhereEq.binding(path / "eventType", enumeratedBinding[ExecutionEventType])

    val WhereSequenceCommandBinding    = WhereOrder.binding(path / "_sequenceCommand", enumeratedBinding[SequenceCommand])
    val WhereStepIdBinding             = WhereEq.binding(path / "_stepId", uidBinding[Step.Id]("step"))
    val WhereStepStageBinding          = WhereOrder.binding(path / "_stepStage", enumeratedBinding[StepStage])
    val WhereDatasetIdBinding          = WhereOrder.binding(path / "_datasetId", gidBinding[Dataset.Id]("dataset"))
    val WhereDatasetStageBinding       = WhereOrder.binding(path / "_datasetStage", enumeratedBinding[DatasetStage])

    lazy val WhereExecutionEventBinding = binding(path)

    ObjectFieldsBinding.rmap {
      case List(
        WhereExecutionEventBinding.List.Option("AND", rAND),
        WhereExecutionEventBinding.List.Option("OR", rOR),
        WhereExecutionEventBinding.Option("NOT", rNOT),
        WhereExecutionEventIdBinding.Option("id", rId),
        WhereVisitIdBinding.Option("visitId", rVisitId),
        WhereObservationIdBinding.Option("observationId", rObservationId),
        WhereTimestampBinding.Option("received", rReceived),
        WhereExecutionEventTypeBinding.Option("eventType", rEventType),
        WhereSequenceCommandBinding.Option("sequenceCommand", rSequenceCommand),
        WhereStepIdBinding.Option("stepId", rStepId),
        WhereStepStageBinding.Option("stepStage", rStepStage),
        WhereDatasetIdBinding.Option("datasetId", rDatasetId),
        WhereDatasetStageBinding.Option("datasetStage", rDatasetStage)

      ) => (rAND, rOR, rNOT, rId, rVisitId, rObservationId, rReceived, rEventType, rSequenceCommand, rStepId, rStepStage, rDatasetId, rDatasetStage).parMapN {
        (AND, OR, NOT, id, vid, oid, received, eventType, sequenceCommand, stepId, stepStage, datasetId, datasetStage) =>
          and(List(
            AND.map(and),
            OR.map(or),
            NOT.map(Not(_)),
            id,
            vid,
            oid,
            received,
            eventType,
            sequenceCommand,
            stepId,
            stepStage,
            datasetId,
            datasetStage
          ).flatten)
      }
    }
  }

}
