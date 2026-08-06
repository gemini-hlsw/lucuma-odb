// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Lifecycle status of a Target-of-Opportunity trigger.  `Requested` is the only
 * non-terminal state; `Accepted`, `Denied` and `Withdrawn` are all terminal.  A
 * re-attempt is a new trigger, never a transition out of a terminal state.
 *
 * This deliberately excludes observation-level concerns.  `Accepted` is terminal
 * regardless of whether the observation ultimately executes, and there is no
 * `Expired` state -- expiration is a function of the observation's timing window,
 * which lives on the observation's own workflow, not here.
 *
 * Defined here in the odb project for now; move to lucuma-core once the design
 * settles.
 */
enum TooTriggerStatus(val tag: String) derives Enumerated:
  case Requested extends TooTriggerStatus("requested")
  case Accepted  extends TooTriggerStatus("accepted")
  case Denied    extends TooTriggerStatus("denied")
  case Withdrawn extends TooTriggerStatus("withdrawn")