// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Lifecycle status of a Target-of-Opportunity trigger.
 *
 * An `Accepted` trigger is what puts its observation into the `Ready` workflow
 * state, and it stays accepted for as long as that holds.  It is not terminal:
 * it may still be withdrawn right up until the observation begins executing,
 * which is the actual point of no return.  `Denied` and `Withdrawn` are
 * terminal, and for the purpose of requesting again are equivalent to never
 * having been triggered at all -- a re-attempt is a new trigger, never a
 * transition out of a terminal state.  At most one trigger per observation may
 * be `Requested` or `Accepted` at a time.
 *
 * This deliberately excludes observation-level concerns.  There is no status
 * meaning "execution has begun": that is already computed from the observation's
 * execution events, and a second copy here would be one more thing to keep in
 * step for no gain.  Nor is there an `Expired` status -- expiration is a
 * function of the observation's timing window, which lives on the observation's
 * own workflow.
 *
 * Defined here in the odb project for now; move to lucuma-core once the design
 * settles.
 */
enum TooTriggerStatus(val tag: String) derives Enumerated:
  case Requested extends TooTriggerStatus("requested")
  case Accepted  extends TooTriggerStatus("accepted")
  case Denied    extends TooTriggerStatus("denied")
  case Withdrawn extends TooTriggerStatus("withdrawn")