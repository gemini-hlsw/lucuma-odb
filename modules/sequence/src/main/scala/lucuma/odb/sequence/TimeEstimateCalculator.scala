// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.data.ProtoStep

trait TimeEstimateCalculator[S, D]:

  def estimateSetup: SetupTime

  def estimateStep(static: S, state: TimeEstimateCalculator.State[D], next: ProtoStep[D]): StepEstimate

object TimeEstimateCalculator:

  /**
   * State kept while computing time estimates.  Figuring out how long a step will
   * take requires information about what has happened in previous steps.
   *
   * @param gcal    last GCAL configuration, if any
   * @param science last science step configuration, if any
   * @param step    last step in general, if any
   *
   * @tparam D instrument dynamic configuration type
   */
  final case class State[D](
    gcal:    Option[StepConfig.Gcal],
    science: Option[StepConfig.Science],
    step:    Option[ProtoStep[D]]
  ):

    def next(step: ProtoStep[D]): State[D] =
      step.stepConfig match
        case g@StepConfig.Gcal(_, _, _, _) => State(g.some, science, step.some)
        case s@StepConfig.Science(_, _)    => State(gcal, s.some, step.some)
        case _                             => copy(step = step.some)

  object State:
    def empty[D]: State[D] =
      State(none, none, none)

  def estimateTimeSpan[S, D](
    calc:   TimeEstimateCalculator[S, D],
    static: S,
    state:  TimeEstimateCalculator.State[D],
    next: List[ProtoStep[D]]
  ): TimeSpan =
    next.foldLeft(TimeSpan.Zero) { (ts, step) =>
      ts +| calc.estimateStep(static, state, step).total
    }