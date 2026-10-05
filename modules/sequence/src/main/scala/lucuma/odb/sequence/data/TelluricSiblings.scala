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
import lucuma.odb.sequence.syntax.all.*
import lucuma.odb.sequence.util.HashBytes

/**
 * What a science observation's group already holds in tellurics: whether the
 * PI has declined one, and for each active telluric whether it is still
 * unobserved and its total: the digest's while unobserved, the original
 * estimate's once visited (none while it has neither).
 */
case class TelluricSiblings(
  declined: Boolean,
  tellurics: List[TelluricSibling]
) derives Eq:

  def unobserved: List[TelluricSibling] =
    tellurics.filter(_.unobserved)

  // Mean total of the tellurics with a digest, what one more is expected to cost.
  def unitCost: Option[CategorizedTime] =
    TelluricSiblings.average(tellurics.flatMap(_.total))

case class TelluricSibling(
  unobserved: Boolean,
  total:      Option[CategorizedTime]
) derives Eq

object TelluricSiblings:

  val None: TelluricSiblings =
    TelluricSiblings(false, Nil)

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

  given HashBytes[CategorizedTime] with
    def hashBytes(a: CategorizedTime): Array[Byte] =
      Array.concat(ChargeClass.values.toList.map(cc => a(cc).hashBytes)*)

  given HashBytes[TelluricSibling] with
    def hashBytes(a: TelluricSibling): Array[Byte] =
      Array.concat(a.unobserved.hashBytes, a.total.hashBytes)

  given HashBytes[TelluricSiblings] with
    def hashBytes(a: TelluricSiblings): Array[Byte] =
      Array.concat(a.declined.hashBytes, a.tellurics.hashBytes)
