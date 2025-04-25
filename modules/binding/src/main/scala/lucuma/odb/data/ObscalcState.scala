// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Obscalc calculation state.
 */
enum ObscalcState(val tag: String) derives Enumerated:

  /**
   * Pending means an update has marked an observation invalid but no workers
   * have started calculating results.
   */
  case Pending     extends ObscalcState("pending")

  /**
   * Like 'Pending' but 'Retry' signifies that at least one attempt to perform
   * the calculation has previously failed.
   */
  case Retry       extends ObscalcState("retry")

  /**
   * An entry in the 'Calculating' state is being processed by a worker.
   */
  case Calculating extends ObscalcState("calculating")

  /**
   * Ready signifies that all update computations have completed and the
   * result is not stale.
   */
  case Ready       extends ObscalcState("ready")