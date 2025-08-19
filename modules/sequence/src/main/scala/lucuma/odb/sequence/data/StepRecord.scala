// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.syntax.eq.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.GcalLampType
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
import lucuma.core.enums.StepType
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.odb.data.StepExecutionState
import lucuma.odb.sequence.syntax.qastate.*
import monocle.Focus
import monocle.Lens

case class StepRecord[D](
  id:               Step.Id,
  atomId:           Atom.Id,
  visitId:          Visit.Id,
  index:            PosInt,
  stepConfig:       StepConfig,
  telescopeConfig:  TelescopeConfig,
  instrument:       Instrument,
  instrumentConfig: D,
  created:          Timestamp,
  interval:         Option[TimestampInterval],
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
    ProtoStep(instrumentConfig, stepConfig, telescopeConfig, observeClass)

  def isGcal: Boolean =
    stepConfig.stepType === StepType.Gcal

  def isFlat: Boolean =
    stepConfig match
      case StepConfig.Gcal(lamp, _, _, _) => lamp.lampType === GcalLampType.Flat
      case StepConfig.SmartGcal(t)        => t === SmartGcalType.Flat
      case _                              => false

  def isArc: Boolean =
    stepConfig match
      case StepConfig.Gcal(lamp, _, _, _) => lamp.lampType === GcalLampType.Arc
      case StepConfig.SmartGcal(t)        => t === SmartGcalType.Arc
      case _                              => false

  def isScience: Boolean =
    stepConfig.stepType === StepType.Science

object StepRecord:

  def telescopeConfig[D]: Lens[StepRecord[D], TelescopeConfig] =
    Focus[StepRecord[D]](_.telescopeConfig)