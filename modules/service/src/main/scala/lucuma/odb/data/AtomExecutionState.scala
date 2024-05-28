// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * The execution state of a recorded atom.
 *
 * @param tag database tag
 */
enum AtomExecutionState(val tag: String) derives Enumerated:

  /** No events have been produced for this step. */
  case NotStarted  extends AtomExecutionState("not_started")

  /** Atom events have arrived, but none are terminal. */
  case Ongoing     extends AtomExecutionState("ongoing")

  /** The atom ended with END_ATOM. */
  case Completed   extends AtomExecutionState("completed")

  /** The atom execution was abandoned without having received an END_ATOM. */
  case Abandoned  extends AtomExecutionState("abandoned")