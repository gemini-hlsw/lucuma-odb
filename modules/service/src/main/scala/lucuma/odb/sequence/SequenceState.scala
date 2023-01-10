// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.data.State
import cats.syntax.option.*
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.StepConfig
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
  def initialConfig: D

  /**
   * Evaluates the stateful computation to produce a value.
   *
   * @param prog definition of the state computation
   * @tparam A type of value ultimately produced
   *
   * @return result of the state computation
   */
  def eval[A](prog: State[D, A]): A =
    prog.runA(initialConfig).value

  def step(f: D => ProtoStep[D]): State[D, ProtoStep[D]] =
    State.inspect[D, ProtoStep[D]](f)

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given telescope configuration.
   */
  def scienceStep(o: Offset): State[D, ProtoStep[D]] =
    step(ProtoStep(_, StepConfig.Science(o)))

  /**
   * Produces a "science" step based upon the current instrument configuration
   * state and the given offset.
   *
   * @param p offset in p
   * @param q offset in q
   */
  def scienceStep(p: Angle, q: Angle): State[D, ProtoStep[D]] =
    scienceStep(Offset(Offset.P(p), Offset.Q(q)))

  /**
   * Generates a GCAL flat based on the current instrument configuration.
   */
  def flatStep: State[D, ProtoStep[D]] =
    step { d =>
      ProtoStep(
        d,
        // TODO: SmartGcal.  This is a placeholder
        StepConfig.Gcal(
          GcalContinuum.QuartzHalogen.some,
          Nil,
          GcalFilter.Nd10,
          GcalDiffuser.Ir,
          GcalShutter.Open
        )
      )
    }

}