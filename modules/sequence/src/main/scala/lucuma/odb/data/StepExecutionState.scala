// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * The execution state of a recorded step.
 *
 * @param tag database tag
 */
enum StepExecutionState(val tag: String) derives Enumerated:

  /** No events have been produced for this step. */
  case NotStarted  extends StepExecutionState("not_started")

  /** Step events have arrived, but none are terminal. */
  case Ongoing     extends StepExecutionState("ongoing")

  /** An ABORT event was received. */
  case Aborted     extends StepExecutionState("aborted")

  /** The step ended normally with END_STEP. */
  case Completed   extends StepExecutionState("completed")

  /** A STOP event was received */
  case Stopped     extends StepExecutionState("stopped")

  /** An ongoing step was abandonded. */
  case Abandoned   extends StepExecutionState("abandoned")

  def fold[A](incomplete: => A, completedFailure: => A, completedSuccess: => A): A =
    this match
      case NotStarted | Ongoing          => incomplete
      case Aborted | Stopped | Abandoned => completedFailure
      case Completed                     => completedSuccess

  def isTerminal: Boolean =
    fold(false, true, true)

end StepExecutionState