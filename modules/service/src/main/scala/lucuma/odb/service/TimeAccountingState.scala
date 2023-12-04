// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Order.catsKernelOrderingForOrder
import cats.data.State
import cats.syntax.functor.*
import lucuma.core.model.sequence.Atom
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

import scala.collection.immutable.SortedMap

opaque type TimeAccountingState = SortedMap[TimestampInterval, TimeAccounting.Context]

object TimeAccountingState {

  val Empty: TimeAccountingState =
    SortedMap.empty

  extension (self: TimeAccountingState) {

    def charge: TimeAccounting.Charge =
      self.foldLeft(TimeAccounting.Charge.Zero) { case (c, (n, x)) =>
        c.sumCharge(x.step.map(_.chargeClass), n.boundedTimeSpan)
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
      stateBetween(interval).foldLeft(Set.empty[Atom.Id]) { case (atoms, (_, ctx)) =>
        ctx.step.fold(atoms) { stepContext => atoms + stepContext.atomId }
      }
  }

  /**
   * Creates a `Charge` value with the amount of time in each category that
   * exists in intervals intersecting with `interval`. Updates the current state
   * to remove these intervals.
   */
  def discountBetween(interval: TimestampInterval): State[TimeAccountingState, TimeAccounting.Charge] =
    State[TimeAccountingState, TimeAccounting.Charge] { tas => (
      tas.stateExcluding(interval),
      tas.stateBetween(interval).charge
    )}

  def discountExcluding(interval: TimestampInterval): State[TimeAccountingState, TimeAccounting.Charge] =
    State[TimeAccountingState, TimeAccounting.Charge] { tas => (
      tas.stateBetween(interval),
      tas.stateExcluding(interval).charge
    )}

  def discountAtom(interval: TimestampInterval): State[TimeAccountingState, TimeAccounting.Charge] =
    State[TimeAccountingState, TimeAccounting.Charge] { tas =>

      val atoms = tas.atomsIn(interval)

      // Compute the discount for all intervals associated with these atoms.
      tas.foldLeft((Empty, TimeAccounting.Charge.Zero)) { case ((state, disc), (curInt, curCtx)) =>
        if (curCtx.step.map(_.atomId).exists(atoms)) (state, disc.sumCharge(curCtx, curInt.boundedTimeSpan))
        else (state + (curInt -> curCtx), disc)
      }

    }

  private[service] def fromSortedContiguousEntries(
    entries: List[(TimestampInterval, TimeAccounting.Context)]
  ): TimeAccountingState =
    SortedMap.from(entries)

}
