// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.derived.*
import cats.syntax.order.*
import lucuma.core.enums.TooActivation
import lucuma.core.util.TimeSpan

/**
 * How long a Target of Opportunity needs to be open for once it is triggered.
 *
 * A ToO cannot state absolute dates -- nobody knows when the alert will come --
 * so it states a length instead, and the trigger opens a real timing window of
 * that length starting at the request.  The stated length is also the ToO's
 * scheduling window for approval purposes, and is deliberately not clipped by
 * the end of the semester: a 24 hour ToO triggered on the last night is still a
 * 24 hour ToO.
 */
enum TooWindow derives Eq:

  /** Open for the given length once triggered. */
  case For(duration: TimeSpan)

  /** Open indefinitely once triggered; the trigger opens no window at all. */
  case Forever

object TooWindow:

  /**
   * The window in effect for an unstated ToO: 24 hours for the activations that
   * demand an immediate response, and Forever for a standard ToO, which waits
   * its turn in the queue like any other observation.  These reproduce what the
   * database did before the ToO window existed, so saying nothing changes
   * nothing.
   */
  def default(activation: TooActivation): TooWindow =
    if activation >= TooActivation.Rapid then For(DefaultRapidWindow) else Forever

  /** Mirrors the SQL `too_default_window()`; the two must not drift. */
  val DefaultRapidWindow: TimeSpan =
    TimeSpan.unsafeFromDuration(java.time.Duration.ofHours(24))

  /** The stated window, else the default for the activation. */
  def effective(activation: TooActivation, stated: Option[TooWindow]): TooWindow =
    stated.getOrElse(default(activation))
