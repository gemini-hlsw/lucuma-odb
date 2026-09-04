// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.util.Enumerated

/**
 * Lifecycle status of a Target-of-Opportunity trigger.
 *
 * A trigger is not requested directly.  It comes into being when a ToO
 * observation is set to the `Ready` workflow state, and is `Withdrawn` when that
 * state is cleared. `Declined` is the one status an observer sets: it records
 * that a trigger was seen and passed over, with a reason, and clears the
 * observation's user state so the observation returns to `Defined`.
 *
 * `Accepted` is the answer the request was asking for: the observatory acted, and
 * the database records it at the first non-slew event, which is when an
 * observation transitions to `Ongoing`.
 *
 * `Superseded` is the one status nobody sets deliberately.  A trigger records
 * the ToO activation it was requested at, and that value never changes: an
 * activation at a different level is a different request, because who is
 * notified, how fast, and what they are expected to do all differ.  So when the
 * observation's activation moves while a request is outstanding, the outstanding
 * row is closed out as `Superseded` and a successor takes its place, linked back
 * to it by `supersedes`.
 *
 * Every status but `Requested` is terminal.  `Declined`, `Withdrawn` and
 * `Superseded` are, for the purpose of triggering again, equivalent to never
 * having been triggered: setting the observation `Ready` once more produces a new
 * trigger rather than reviving an old one.  They are kept distinct because they
 * answer different questions -- `Withdrawn` means the PI took it back,
 * `Superseded` means the same request came back wearing a different activation.
 * `Accepted` is terminal in a stronger sense: the request was answered, so a
 * further attempt at the observation is a new request rather than a reopening of
 * this one.
 *
 * Defined here in the odb project for now; move to lucuma-core once the design
 * settles.
 */
enum TooTriggerStatus(val tag: String) derives Enumerated:
  case Requested  extends TooTriggerStatus("requested")
  case Accepted   extends TooTriggerStatus("accepted")
  case Declined   extends TooTriggerStatus("declined")
  case Withdrawn  extends TooTriggerStatus("withdrawn")
  case Superseded extends TooTriggerStatus("superseded")
