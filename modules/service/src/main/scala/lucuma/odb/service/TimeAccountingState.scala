// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.State
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.order.*
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

import scala.collection.immutable.SortedMap

/**
 * TimeAccountingState tracks time intervals associated with a visit and
 * potentially a step.  Intervals are not overlapping.
 */
sealed class TimeAccountingState private (val toMap: SortedMap[TimestampInterval, TimeAccounting.Context]) {

  import TimeAccountingState.Empty

  def toList: List[(TimestampInterval, TimeAccounting.Context)] =
    toMap.toList

  /**
   * Calculates the time accounting charge associated with this state.
   */
  def charge: CategorizedTime =
    toMap.foldLeft(CategorizedTime.Zero) { case (c, (n, x)) =>
      c.sumCharge(x.chargeClass, n.boundedTimeSpan)
    }

  /**
   * Looks up the context at this given `Timestamp`, assuming it is covered by
   * the state.
   */
  def contextAt(t: Timestamp): Option[TimeAccounting.Context] =
    toMap
      .maxBefore(TimestampInterval.empty(t))
      .collect { case (interval, ctx) if interval.contains(t) => ctx }
      .orElse(
        // if t happens to be the start of an interval, `[t, t)` will
        // sort before it and min "after" will start at `t`.
        toMap
          .minAfter(TimestampInterval.empty(t))
          .collect { case (interval, ctx) if interval.contains(t) => ctx }
      )

  private def mod(f: SortedMap[TimestampInterval, TimeAccounting.Context] => SortedMap[TimestampInterval, TimeAccounting.Context]): TimeAccountingState =
    new TimeAccountingState(f(toMap))

  /**
   * Extracts the portion of the state that happens before the given timestamp
   * (if any).  If the timestamp occurs in the middle of an interval, it will
   * be split such that the remaining portion starting at `t` is excluded.
   */
  def until(t: Timestamp): TimeAccountingState =
    mod(
      _.maxBefore(TimestampInterval.empty(t))
       .fold(Empty.toMap) { (k, v) =>
         if (k.contains(t)) toMap.rangeUntil(k) ++ k.minus(TimestampInterval.from(t)).tupleRight(v)
         else toMap.rangeTo(k)
       }
    )

  /**
   * Extracts the portion of the state that happens on or after the given
   * timestamp (if any).  If the timestamp occurs in the middle of an interval,
   * it will be split such that the initial portion until `t` is excluded.
   */
  def from(t: Timestamp): TimeAccountingState =
    mod(
      _.maxBefore(TimestampInterval.empty(t))
       .fold(toMap) { (k, v) =>
         val tail = toMap.rangeFrom(TimestampInterval.empty(t))
         if (k.contains(t)) tail ++ k.minus(TimestampInterval.until(t)).tupleRight(v)
         else tail
       }
    )

  /**
   * Extracts the portion of the state that intersects with the given interval
   * (if any).  If `interval` partially overlaps one or more intervals in the
   * state, they will be split as appropriate leaving only the portion that
   * falls within `interval` bounds.
   */
  def between(interval: TimestampInterval): TimeAccountingState =
    if (interval.isEmpty) Empty else from(interval.start).until(interval.end)

  /**
   * Extracts the state that does not intersect with `interval` (if any).  If
   * `interval` partially overlaps one or more intervals in the state, they will
   * be split as appropriate leaving only the portion that does not fall within
   * `interval` bounds.
   */
  def excluding(interval: TimestampInterval): TimeAccountingState =
    if (interval.isEmpty) this
    else new TimeAccountingState(until(interval.start).toMap ++ from(interval.end).toMap)

  /**
   * Returns the set of `Atom.Id` that are associated with state intervals which
   * intersect with `interval`.
   */
  def atomsIn(interval: TimestampInterval): Set[Atom.Id] =
    between(interval).toMap.foldLeft(Set.empty[Atom.Id]) { case (atoms, (_, ctx)) =>
      ctx.step.fold(atoms) { stepContext => atoms + stepContext.atomId }
    }

  def isEmpty: Boolean =
    toMap.isEmpty

  def nonEmpty: Boolean =
    toMap.nonEmpty

  override def equals(that: Any): Boolean =
    that match {
      case s: TimeAccountingState => toMap === s.toMap
      case _                      => false
    }

  override def hashCode: Int =
    toMap.hashCode() * 31

  override def toString: String =
    s"TimeAccountingState(${toMap.mkString(", ")})"
}

object TimeAccountingState {

  val Empty: TimeAccountingState =
    new TimeAccountingState(SortedMap.empty)

  given Eq[TimeAccountingState] =
    Eq.by(_.toMap)

  /**
   * Creates a `CategorizedTime` value with the amount of time in each category
   * that overlaps with `interval`. Updates the current state to remove the time
   * in `interval`.
   *
   * For example, this can be used to subtract weather loss where `interval` is
   * the bad weather time that should not be charged.
   */
  def discountBetween(interval: TimestampInterval): State[TimeAccountingState, CategorizedTime] =
    State[TimeAccountingState, CategorizedTime] { tas =>
      (tas.excluding(interval), tas.between(interval).charge)
    }

  /**
   * Creates a `CategorizedTime` value with the amount of time in each category
   * that does not overlap with `interval`.  Updates the current state to only
   * the time that occurs during `interval`.
   *
   * For example, this can be used to subtract daylight where `interval` is the
   * night time between twilight boundaries.
   */
  def discountExcluding(interval: TimestampInterval): State[TimeAccountingState, CategorizedTime] =
    State[TimeAccountingState, CategorizedTime] { tas =>
     (tas.between(interval), tas.excluding(interval).charge)
    }

  /**
   * Creates a `CategorizedTime` value with the amount of time associated with
   * any atom whose execution overlaps `interval`.  Updates the current state to
   * remove this time.
   *
   * For example, this can be used to remove an atom that produced a dataset
   * with a failing QA state.
   */
  def discountAtoms(interval: TimestampInterval): State[TimeAccountingState, CategorizedTime] =
    State[TimeAccountingState, CategorizedTime] { tas =>

      val atoms = tas.atomsIn(interval)

      // Compute the discount for all intervals associated with these atoms.
      tas.toMap.foldLeft((Empty, CategorizedTime.Zero)) { case ((state, disc), (curInt, curCtx)) =>
        if (curCtx.step.map(_.atomId).exists(atoms)) (state, disc.sumCharge(curCtx.chargeClass, curInt.boundedTimeSpan))
        else (state.mod(_ + (curInt -> curCtx)), disc)
      }

    }

  /**
   * Creates a TimeAccountingState from a sequence of events, assuming they are
   * sorted.
   */
  def unsafeFromEvents(
    chargeClass: ChargeClass,
    visitId:     Visit.Id,
    events:      Seq[TimeAccounting.Event]
  ): TimeAccountingState =
    Stream
      .emits(events)
      .through(eventStreamPipe(chargeClass, visitId))
      .toList
      .head

  /**
   * A pipe that processes the event stream into a single-element stream
   * containing the corresponding `TimeAccountingState`.
   */
  def eventStreamPipe[F[_]](
    chargeClass: ChargeClass,
    visitId: Visit.Id
  ): Pipe[F, TimeAccounting.Event, TimeAccountingState] = {

    // The events must be presented in order or this won't work.
    val validateSortOrder: Pipe[F, TimeAccounting.Event, TimeAccounting.Event] =
      _.zipWithNext
       .flatMap {
         case (cur, Some(next)) if (cur.timestamp > next.timestamp) => throw new RuntimeException("Events out of order!")
         case (cur, _)                                              => Stream.emit(cur)
       }

    // Pipe that turns time accounting events into interval -> context pairs
    // so that they can be used to create TimeAccountingState.  We merge all
    // the adjacent ones with the same context into a single entry covering
    // the entire time.
    val entriesPipe: Pipe[F, TimeAccounting.Event, (TimestampInterval, TimeAccounting.Context)] =
      _.groupAdjacentBy(_.context)
       .map { case (ctx, events) =>
          val head     = events.head.map(_.timestamp)
          val last     = events.last.map(_.timestamp)
          val interval =
            (head, last).mapN(TimestampInterval.between)
                        .getOrElse(sys.error("Stream.groupAdjacentBy produced an empty Chunk!"))

          interval -> ctx
       }

    // Pipe that fills in the gaps between steps with an interval -> context
    // pair (albeit with a None step context).
    val contiguousPipe: Pipe[F, (TimestampInterval, TimeAccounting.Context), (TimestampInterval, TimeAccounting.Context)] =
      _.zipWithNext
       .flatMap {
         case ((interval0, ctx0), None)                  =>
           Stream.emit(interval0 -> ctx0)
         case ((interval0, ctx0), Some(interval1, ctx1)) =>
           if (interval0.abuts(interval1)) Stream.emit(interval0 -> ctx0)
           else Stream(
             interval0 -> ctx0,
             TimestampInterval.between(interval0.end, interval1.start) -> TimeAccounting.Context(visitId, chargeClass, None)
           )
       }

    _.through(validateSortOrder)
     .through(entriesPipe)
     .through(contiguousPipe)
     .fold(Empty.toMap) { case (state, (interval, ctx)) =>
       state + (interval -> ctx)
     }
     .map(new TimeAccountingState(_))

  }
}
