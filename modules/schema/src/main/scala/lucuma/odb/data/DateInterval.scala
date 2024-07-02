// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.order.*
import org.typelevel.cats.time.*

import java.time.LocalDate

/**
 * Interval between a start date and an end date.
 */
sealed class DateInterval private (val start: LocalDate, val end: LocalDate):
  assert(start <= end, s"start date ($start) must be <= end date ($end)")

  def isEmpty: Boolean =
    start === end

  def isNonEmpty: Boolean =
    !isEmpty

object DateInterval:

  given Eq[DateInterval] =
    Eq.by(a => (a.start, a.end))

  /**
   * Creates an interval between the two dates, swapping the order if
   * necessary.  If d0 comes before d1, the interval is [d0, d1).  Otherwise
   * it is [d1, d0).
   */
  def between(d0: LocalDate, d1: LocalDate): DateInterval =
    if (d0 <= d1) new DateInterval(d0, d1) else new DateInterval(d1, d0)