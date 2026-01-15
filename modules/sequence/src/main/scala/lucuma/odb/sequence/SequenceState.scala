// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.data.State
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SmartGcalType.Arc
import lucuma.core.enums.SmartGcalType.Flat
import lucuma.core.enums.StepGuideState.Enabled
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.SmartGcal
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.odb.sequence.data.ProtoStep

/**
 * Sequence generation helper trait.
 */
trait SequenceState[D] {

  /**
   * Sequence generation involves defining edits to the dynamic instrument
   * configuration.  The starting point instrument configuration, or
   * initialConfig, defines the initial state and subsequent steps edit it
   * in a `State` "program".
   */
  def initialDynamicConfig: D

  /**
   * Evaluates the stateful computation to produce a value.
   *
   * @param prog definition of the state computation
   * @tparam A type of value ultimately produced
   *
   * @return result of the state computation
   */
  def eval[A](prog: State[D, A]): A =
    prog.runA(initialDynamicConfig).value

  def step(f: D => ProtoStep[D]): State[D, ProtoStep[D]] =
    State.inspect[D, ProtoStep[D]](f)

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given telescope configuration.
   */
  def scienceStep(t: TelescopeConfig, c: ObserveClass): State[D, ProtoStep[D]] =
    step(ProtoStep(_, StepConfig.Science, t, c))

  def scienceStep(o: Offset, c: ObserveClass): State[D, ProtoStep[D]] =
    scienceStep(TelescopeConfig(o, Enabled), c)

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given offset.
   *
   * @param p offset in p
   * @param q offset in q
   */
  def scienceStep(p: Angle, q: Angle, c: ObserveClass): State[D, ProtoStep[D]] =
    scienceStep(TelescopeConfig(Offset(Offset.P(p), Offset.Q(q)), Enabled), c)

  /**
   * Generates a GCAL flat based on the current instrument configuration.
   */
  def flatStep(t: TelescopeConfig, c: ObserveClass): State[D, ProtoStep[D]] =
    step(ProtoStep(_, SmartGcal(Flat), t, c))

  /**
   * Generates a GCAL arc based on the current instrument configuration.
   */
  def arcStep(t: TelescopeConfig, c: ObserveClass): State[D, ProtoStep[D]] =
    step(ProtoStep(_, SmartGcal(Arc), t, c))

}
