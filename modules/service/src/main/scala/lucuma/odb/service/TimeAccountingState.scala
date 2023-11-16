// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.State
import cats.syntax.functor.*
import lucuma.core.enums.ChargeClass
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

import scala.collection.immutable.SortedMap

opaque type TimeAccountingState = SortedMap[TimestampInterval, TimeAccountingState.Value]

object TimeAccountingState {

  val Empty: TimeAccountingState =
    SortedMap.empty

  case class Value(
    atomId:      Atom.Id,
    chargeClass: ChargeClass
  )

  extension (self: TimeAccountingState) {

    def categorizedTime: CategorizedTime =
      self.foldLeft(CategorizedTime.Zero) { case (t, (n, v)) =>
        t.sumCharge(v.chargeClass, n.boundedTimeSpan)
      }

    def stateUntil(t: Timestamp): TimeAccountingState =
      self
        .maxBefore(TimestampInterval.empty(t))
        .fold(Empty) { (k, v) =>
          if (k.contains(t)) self.rangeUntil(k) ++ k.minus(TimestampInterval.from(t)).tupleRight(v)
          else self.rangeTo(k)
        }

    def stateFrom(t: Timestamp): TimeAccountingState =
      self
        .maxBefore(TimestampInterval.empty(t))
        .fold(self) { (k, v) =>
          val tail = self.rangeFrom(TimestampInterval.empty(t))
          if (k.contains(t)) tail ++ k.minus(TimestampInterval.until(t)).tupleRight(v)
          else tail
        }

    def stateBetween(interval: TimestampInterval): TimeAccountingState =
      self.stateFrom(interval.start).stateUntil(interval.end)

    def stateExcluding(interval: TimestampInterval): TimeAccountingState =
      self.stateUntil(interval.start).stateFrom(interval.end)

    def atomsIn(interval: TimestampInterval): Set[Atom.Id] =
      stateBetween(interval).foldLeft(Set.empty[Atom.Id])(_ + _._2.atomId)
  }

  /**
   * Creates a `CategorizedTime` value with the amount of time in each category
   * that exists in intervals intersecting with `interval`. Updates the current
   * state to remove these intervals.
   */
  def discountBetween(interval: TimestampInterval): State[TimeAccountingState, CategorizedTime] =
    State[TimeAccountingState, CategorizedTime] { tas => (
      tas.stateExcluding(interval),
      tas.stateBetween(interval).categorizedTime
    )}

  def discountExcluding(interval: TimestampInterval): State[TimeAccountingState, CategorizedTime] =
    State[TimeAccountingState, CategorizedTime] { tas => (
      tas.stateBetween(interval),
      tas.stateExcluding(interval).categorizedTime
    )}

  def discountAtom(interval: TimestampInterval): State[TimeAccountingState, CategorizedTime] =
    State[TimeAccountingState, CategorizedTime] { tas =>

      val atoms = tas.atomsIn(interval)

      // Compute the discount for all intervals associated with these atoms.
      tas.foldLeft((Empty, CategorizedTime.Zero)) { case ((state, disc), (curInt, curVal)) =>
        if (atoms(curVal.atomId)) (state, disc.sumCharge(curVal.chargeClass, curInt.boundedTimeSpan))
        else (state + (curInt -> curVal), disc)
      }

    }

//  def fromEvents[F[_]](s: Stream[F, ExecutionEvent]): F[TimeAccountingState] =
//    s.zip(s.tail)
//     .map { (e0, e1) =>
//       val interval = TimestampInterval.between(e0.received, e1.received)
//       val atomId   = e0.
//       e1.received
//     }
}
