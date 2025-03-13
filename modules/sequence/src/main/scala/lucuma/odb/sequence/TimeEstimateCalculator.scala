// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence

import cats.syntax.option.*
import lucuma.core.math.Offset
import lucuma.core.model.sequence.SetupTime
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepEstimate
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.data.ProtoStep

/**
 * Estimates the cost of setup and step execution.
 *
 * @tparam S static config type
 * @tparam D dynamic config type
 */
trait TimeEstimateCalculator[S, D]:

  /**
   * Provides a rough estimate of the setup time, which includes acquisition.
   */
  def estimateSetup: SetupTime

  /**
   * Provides an estimate of the cost for executing the 'next' step, given the
   * provided 'past' state.
   */
  def estimateStep(static: S, last: TimeEstimateCalculator.Last[D], next: ProtoStep[D]): StepEstimate

object TimeEstimateCalculator:

  /**
   * State kept while computing time estimates.  Figuring out how long a step will
   * take requires information about what has happened in previous steps.
   *
   * @param gcal    last GCAL configuration, if any
   * @param step    last step in general, if any
    *
   * @tparam D instrument dynamic configuration type
   */
  final case class Last[D](
    gcal:    Option[StepConfig.Gcal],
    step:    Option[ProtoStep[D]]
  ):

    def offset: Offset =
      step.map(_.telescopeConfig.offset).getOrElse(Offset.Zero)

    def next(step: ProtoStep[D]): Last[D] =
      step.stepConfig match
        case g@StepConfig.Gcal(_, _, _, _) => Last(g.some, step.some)
        case _                             => copy(step = step.some)

  object Last:
    def empty[D]: Last[D] =
      Last(none, none)

  def estimateTimeSpan[S, D](
    calc:   TimeEstimateCalculator[S, D],
    static: S,
    last:   TimeEstimateCalculator.Last[D],
    next:   List[ProtoStep[D]]
  ): TimeSpan =
    next.foldLeft(TimeSpan.Zero) { (ts, step) =>
      ts +| calc.estimateStep(static, last, step).total
    }
