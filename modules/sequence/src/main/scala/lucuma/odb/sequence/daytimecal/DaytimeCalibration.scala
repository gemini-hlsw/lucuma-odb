// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.math.Offset
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SequenceGenerator
import lucuma.odb.sequence.StepTimeEstimateCalculator
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.util.AtomBuilder

import java.util.UUID

/**
 * Generates a daytime calibration sequence from its Calibration Sets: one
 * atom per set holding `count` identical DAY_CAL steps, never guided.
 * Nothing here is instrument-specific.
 */
object DaytimeCalibration:

  private val Telescope: TelescopeConfig =
    TelescopeConfig(Offset.Zero, StepGuideState.Disabled)

  private def stepConfig(oid: Observation.Id, index: Int, set: CalibrationSet[?]): Either[OdbError, StepConfig] =
    (set.kind, set.gcal) match
      case (CalibrationSetType.Bias, _)                                => StepConfig.Bias.asRight
      case (CalibrationSetType.Dark, _)                                => StepConfig.Dark.asRight
      case (CalibrationSetType.DaytimeObserve, _)                      => StepConfig.Science.asRight
      case (CalibrationSetType.Flat | CalibrationSetType.Arc, Some(g)) => g.asRight
      case (k, None)                                                   =>
        OdbError.SequenceUnavailable(oid, s"Calibration set $index (${k.name}) has no GCAL configuration.".some).asLeft

  def protoAtom[D](oid: Observation.Id, index: Int, set: CalibrationSet[D]): Either[OdbError, ProtoAtom[ProtoStep[D]]] =
    stepConfig(oid, index, set).map: sc =>
      val step = ProtoStep(set.setup, sc, Telescope, ObserveClass.DayCal)
      ProtoAtom(
        NonEmptyString.from(s"${set.kind.name} x${set.count.value}").toOption,
        NonEmptyList(step, List.fill(set.count.value - 1)(step))
      )

  def generator[S, D](
    oid:       Observation.Id,
    estimator: StepTimeEstimateCalculator[S, D],
    static:    S,
    namespace: UUID,
    sets:      List[CalibrationSet[D]]
  ): Either[OdbError, SequenceGenerator[D]] =
    sets.zipWithIndex.traverse((s, i) => protoAtom(oid, i, s)).map: atoms =>
      new SequenceGenerator[D]:
        override def generate =
          AtomBuilder
            .instantiate(estimator, static, namespace, SequenceType.Science)
            .buildStream(Stream.emits(atoms))

  def executionConfig[S, D](
    oid:       Observation.Id,
    estimator: StepTimeEstimateCalculator[S, D],
    static:    S,
    namespace: UUID,
    sets:      List[CalibrationSet[D]]
  ): Either[OdbError, StreamingExecutionConfig[Pure, S, D]] =
    generator(oid, estimator, static, namespace, sets).map: g =>
      StreamingExecutionConfig(static, SequenceGenerator.empty.generate, g.generate)
