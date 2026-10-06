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
import lucuma.odb.sequence.util.HashBytes

/**
 * What a science observation's group already holds in tellurics: whether the
 * PI has declined one, and for each active telluric whether it is still
 * unobserved and its total: the digest's while unobserved, the original
 * estimate's once visited (none while it has neither).
 */
case class CalibrationGroupTellurics(
  declined: Boolean,
  tellurics: List[CalibrationGroupTelluric]
) derives Eq:

  def unobserved: List[CalibrationGroupTelluric] =
    tellurics.filter(_.unobserved)

  // Mean total of the tellurics that have one, what one more is expected to cost.
  def unitCost: Option[CategorizedTime] =
    CalibrationGroupTellurics.average(tellurics.flatMap(_.total))

case class CalibrationGroupTelluric(
  unobserved: Boolean,
  total:      Option[CategorizedTime]
) derives Eq

object CalibrationGroupTellurics:

  val Empty: CalibrationGroupTellurics =
    CalibrationGroupTellurics(false, Nil)

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

  given HashBytes[CalibrationGroupTelluric] =
    HashBytes.by2(_.unobserved, _.total)

  given HashBytes[CalibrationGroupTellurics] =
    HashBytes.by2(_.declined, _.tellurics)
