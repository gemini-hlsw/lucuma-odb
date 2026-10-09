// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.data.NonEmptyList
import cats.syntax.parallel.*
import grackle.syntax.*
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.model.Observation
import lucuma.core.model.ObservationReference
import lucuma.core.model.sequence.Step
import lucuma.odb.graphql.binding.*

final case class InsertSmartGcalInput(
  observationId:  Option[Observation.Id],
  observationRef: Option[ObservationReference],
  sequenceType:   SequenceType,
  smartGcalTypes: NonEmptyList[SmartGcalType],
  afterStepId:    Option[Step.Id]
)

object InsertSmartGcalInput:

  val Binding: Matcher[InsertSmartGcalInput] =
    ObjectFieldsBinding.rmap:
      case List(
        ObservationIdBinding.Option("observationId", rObservationId),
        ObservationReferenceBinding.Option("observationReference", rObservationRef),
        SequenceTypeBinding.Option("sequenceType", rSequenceType),
        SmartGcalTypeBinding.List("smartGcalTypes", rSmartGcalTypes),
        StepIdBinding.Option("afterStepId", rAfterStepId)
      ) =>
        (
          rObservationId,
          rObservationRef,
          rSequenceType.map(_.getOrElse(SequenceType.Science)),
          rSmartGcalTypes.flatMap(NonEmptyList.fromList(_).toResult("At least one smart GCAL type is required.")),
          rAfterStepId
        ).parMapN(InsertSmartGcalInput.apply)
