// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.Monoid
import cats.Order.catsKernelOrderingForOrder
import cats.syntax.foldable.*
import cats.syntax.option.*
import lucuma.core.enums.ChargeClass
import lucuma.core.util.TimeSpan

import scala.collection.immutable.SortedMap

case class TimeCharges(
  charged:   SortedMap[ChargeClass, TimeSpan],
  uncharged: TimeSpan
) {

  def +|(other: TimeCharges): TimeCharges =
    TimeCharges(
      charged.foldLeft(other.charged) { case (m, (k, v)) =>
        m.updatedWith(k)(_.foldLeft(v)(_ +| _).some)
      },
      uncharged +| other.uncharged
    )

  def apply(chargeClass: ChargeClass): TimeSpan =
    charged.getOrElse(chargeClass, TimeSpan.Zero)

}

object TimeCharges {

  val Zero: TimeCharges =
    TimeCharges(SortedMap.empty, TimeSpan.Zero)

  given Monoid[TimeCharges] =
    Monoid.instance(Zero, _ +| _)

  given Eq[TimeCharges] =
    Eq.by { a => (a.charged, a.uncharged) }

}
