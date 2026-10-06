// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.TimeSpan

/**
 * The tellurics in a science observation's group: for each, whether it is
 * still unobserved, whether the PI declined it, and its total: the digest's
 * while unobserved, the original estimate's once visited (none while it has
 * neither).
 */
case class CalibrationGroupTellurics(
  tellurics: List[CalibrationGroupTelluric]
) derives Eq:

  // Covers a slot of the next visit, declined or not.
  def unobserved: List[CalibrationGroupTelluric] =
    tellurics.filter(_.unobserved)

  // Unobserved and still to be observed.
  def existing: List[CalibrationGroupTelluric] =
    unobserved.filterNot(_.declined)

  // Mean total of the active tellurics that have one, what one more is expected to cost.
  def unitCost: Option[CategorizedTime] =
    CalibrationGroupTellurics.average(tellurics.filterNot(_.declined).flatMap(_.total))

case class CalibrationGroupTelluric(
  unobserved: Boolean,
  declined:   Boolean,
  total:      Option[CategorizedTime]
) derives Eq

object CalibrationGroupTellurics:

  val Empty: CalibrationGroupTellurics =
    CalibrationGroupTellurics(Nil)

  def average(totals: List[CategorizedTime]): Option[CategorizedTime] =
    totals match
      case Nil => none
      case ts  =>
        val sum = ts.combineAll
        CategorizedTime(
          ChargeClass.values.toList.map: cc =>
            cc -> TimeSpan.unsafeFromMicroseconds(sum(cc).toMicroseconds / ts.size)
          *
        ).some
