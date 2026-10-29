// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.daytimecal

import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.string.NonEmptyString
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.CalibrationSetType
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SmartGcalType
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
 * atom per set, never guided.  A manual set's atom holds `count` identical
 * DAY_CAL steps; a smart set's atom holds one smart GCAL step the caller
 * expands with the instrument's SmartGcalExpander before building atoms.
 * Nothing here is instrument-specific.
 */
object DaytimeCalibration:

  private val Telescope: TelescopeConfig =
    TelescopeConfig(Offset.Zero, StepGuideState.Disabled)

  private def unavailable(oid: Observation.Id, index: Int, kind: CalibrationSetType, msg: String): OdbError =
    OdbError.SequenceUnavailable(oid, s"Calibration set $index (${kind.tag.toUpperCase}) $msg".some)

  /** The complaint for a smart set whose lookup failed. */
  def lookupFailed(oid: Observation.Id, index: Int, kind: CalibrationSetType, error: String): OdbError =
    unavailable(oid, index, kind, s"could not be resolved: $error.")

  private def stepConfig(oid: Observation.Id, index: Int, set: CalibrationSet[?]): Either[OdbError, StepConfig] =
    (set.kind, set.gcal) match
      case (CalibrationSetType.Bias, _)                                => StepConfig.Bias.asRight
      case (CalibrationSetType.Dark, _)                                => StepConfig.Dark.asRight
      case (CalibrationSetType.DaytimeObserve, _)                      => StepConfig.Science.asRight
      case (CalibrationSetType.SmartFlat, _)                           => StepConfig.SmartGcal(SmartGcalType.Flat).asRight
      case (CalibrationSetType.SmartArc, _)                            => StepConfig.SmartGcal(SmartGcalType.Arc).asRight
      case (CalibrationSetType.DayBaseline, _)                         => StepConfig.SmartGcal(SmartGcalType.DayBaseline).asRight
      case (CalibrationSetType.Flat | CalibrationSetType.Arc, Some(g)) => g.asRight
      case (k, None)                                                   =>
        unavailable(oid, index, k, "has no GCAL configuration.").asLeft

  private def stepCount(oid: Observation.Id, index: Int, set: CalibrationSet[?]): Either[OdbError, Int] =
    (set.kind.isSmart, set.count) match
      case (true, _)        => 1.asRight
      case (false, Some(n)) => n.value.asRight
      case (false, None)    => unavailable(oid, index, set.kind, "has no exposure count.").asLeft

  private def description(set: CalibrationSet[?], count: Int): Option[NonEmptyString] =
    val s = if set.kind.isSmart then set.kind.name else s"${set.kind.name} x$count"
    NonEmptyString.from(s).toOption

  /** The step a smart set looks up, or the step a manual set repeats. */
  def protoStep[D](oid: Observation.Id, index: Int, set: CalibrationSet[D]): Either[OdbError, ProtoStep[D]] =
    stepConfig(oid, index, set).map(sc => ProtoStep(set.setup, sc, Telescope, ObserveClass.DayCal))

  def protoAtom[D](oid: Observation.Id, index: Int, set: CalibrationSet[D]): Either[OdbError, ProtoAtom[ProtoStep[D]]] =
    (protoStep(oid, index, set), stepCount(oid, index, set)).mapN: (step, count) =>
      ProtoAtom(description(set, count), NonEmptyList(step, List.fill(count - 1)(step)))

  def protoAtoms[D](oid: Observation.Id, sets: List[CalibrationSet[D]]): Either[OdbError, List[ProtoAtom[ProtoStep[D]]]] =
    sets.zipWithIndex.traverse((s, i) => protoAtom(oid, i, s))

  def generator[S, D](
    estimator: StepTimeEstimateCalculator[S, D],
    static:    S,
    namespace: UUID,
    atoms:     List[ProtoAtom[ProtoStep[D]]]
  ): SequenceGenerator[D] =
    new SequenceGenerator[D]:
      override def generate =
        AtomBuilder
          .instantiate(estimator, static, namespace, SequenceType.Science)
          .buildStream(Stream.emits(atoms))

  /** The execution config of already expanded atoms: no smart steps remain. */
  def executionConfig[S, D](
    estimator: StepTimeEstimateCalculator[S, D],
    static:    S,
    namespace: UUID,
    atoms:     List[ProtoAtom[ProtoStep[D]]]
  ): StreamingExecutionConfig[Pure, S, D] =
    StreamingExecutionConfig(static, SequenceGenerator.empty.generate, generator(estimator, static, namespace, atoms).generate)
