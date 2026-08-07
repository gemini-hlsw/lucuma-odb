// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Lifecycle status of a Target-of-Opportunity trigger.
 *
 * A trigger is not requested directly.  It comes into being when a ToO
 * observation is set to the `Ready` workflow state, and is `Withdrawn` when that
 * state is cleared -- the database maintains the correspondence, so the two can
 * never disagree.  `Declined` is the one status an observer sets: it records
 * that a trigger was seen and passed over, with a reason, and clears the
 * observation's user state so the observation returns to `Defined`.
 *
 * `Declined` and `Withdrawn` are terminal, and for the purpose of triggering
 * again are equivalent to never having been triggered: setting the observation
 * `Ready` once more produces a new trigger rather than reviving an old one.
 *
 * There is deliberately no status meaning "approved".  The proposal's ToO
 * activation ceiling, frozen at acceptance, is the authorization; a second
 * per-trigger approval would only add latency.  Nor is there one meaning
 * "execution has begun" -- that is computed from the observation's execution
 * events, and the workflow already prevents a running observation from leaving
 * `Ongoing`, so a live trigger cannot be withdrawn out from under it.
 *
 * Defined here in the odb project for now; move to lucuma-core once the design
 * settles.
 */
enum TooTriggerStatus(val tag: String) derives Enumerated:
  case Requested extends TooTriggerStatus("requested")
  case Declined  extends TooTriggerStatus("declined")
  case Withdrawn extends TooTriggerStatus("withdrawn")
