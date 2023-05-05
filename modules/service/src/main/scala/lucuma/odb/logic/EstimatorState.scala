// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.syntax.option.*
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.sequence.data.ProtoStep

final case class EstimatorState[D](
  gcal:    Option[StepConfig.Gcal],
  science: Option[StepConfig.Science],
  step:    Option[ProtoStep[D]]
) {

  def next(step: ProtoStep[D]): EstimatorState[D] =
    step.stepConfig match {
      case g@StepConfig.Gcal(_, _, _, _) => EstimatorState(g.some, science, step.some)
      case s@StepConfig.Science(_)       => EstimatorState(gcal, s.some, step.some)
      case _                             => copy(step = step.some)
    }

}

object EstimatorState {

  def empty[D]: EstimatorState[D] =
    EstimatorState(none, none, none)

}

