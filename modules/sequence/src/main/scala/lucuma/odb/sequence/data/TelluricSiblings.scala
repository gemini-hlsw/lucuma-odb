// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence
package data

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

/**
 * What a science observation's group already holds in tellurics: how many are
 * still unobserved, whether the PI has declined one, and the average total of
 * those with a digest, which is what one more telluric is expected to cost.
 */
case class TelluricSiblings(
  unobserved: NonNegInt,
  declined:   Boolean,
  unitCost:   Option[CategorizedTime]
) derives Eq

object TelluricSiblings:

  val None: TelluricSiblings =
    TelluricSiblings(NonNegInt.MinValue, false, scala.None)

  /** Mean per charge class; `None` for an empty list. */
  def average(totals: List[CategorizedTime]): Option[CategorizedTime] =
    totals match
      case Nil => scala.None
      case ts  =>
        val sum = ts.combineAll
        CategorizedTime(
          ChargeClass.values.toList.map: cc =>
            cc -> TimeSpan.unsafeFromMicroseconds(sum(cc).toMicroseconds / ts.size)
          *
        ).some

  given HashBytes[CategorizedTime] with
    def hashBytes(a: CategorizedTime): Array[Byte] =
      Array.concat(ChargeClass.values.toList.map(cc => a(cc).hashBytes)*)

  given HashBytes[TelluricSiblings] with
    def hashBytes(a: TelluricSiblings): Array[Byte] =
      Array.concat(a.unobserved.hashBytes, a.declined.hashBytes, a.unitCost.hashBytes)
