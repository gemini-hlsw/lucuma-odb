// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.util.Timestamp
import lucuma.odb.data.StepExecutionState
import lucuma.odb.sequence.syntax.qastate.*

case class StepRecord[D](
  id:               Step.Id,
  atomId:           Atom.Id,
  visitId:          Visit.Id,
  index:            PosInt,
  stepConfig:       StepConfig,
  instrument:       Instrument,
  instrumentConfig: D,
  created:          Timestamp,
  sequenceType:     SequenceType,
  observeClass:     ObserveClass,
  executionState:   StepExecutionState,
  qaState:          Option[DatasetQaState]
):

  def isAcquisitionSequence: Boolean =
    sequenceType === SequenceType.Acquisition

  def isScienceSequence: Boolean =
    sequenceType === SequenceType.Science

  def successfullyCompleted: Boolean =
    DatasetQaState.orPassing(qaState).isPassing &&
      executionState.fold(false, false, true)

  def protoStep: ProtoStep[D] =
    ProtoStep(instrumentConfig, stepConfig, observeClass)