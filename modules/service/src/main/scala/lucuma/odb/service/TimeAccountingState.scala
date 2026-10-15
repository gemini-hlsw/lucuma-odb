// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.Order.catsKernelOrderingForOrder
import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.monoid.*
import cats.syntax.option.*
import cats.syntax.order.*
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SetupKind
import lucuma.core.enums.SlewStage
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.SetupTimeBreakdown
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

/**
 * TimeAccountingState tracks time intervals associated with a visit and
 * potentially a step.  Intervals are not overlapping.
 */
sealed class TimeAccountingState private (val toMap: SortedMap[TimestampInterval, TimeAccounting.Context]) {

  import TimeAccountingState.Empty

  /**
   * Timestamp of the start of the first interval in the state, if any.
   */
  def start: Option[Timestamp] =
    toMap.headOption.map(_._1.start)

  /**
   * Timestamp of the end of the last interval in the state, if any.
   */
  def end: Option[Timestamp] =
    toMap.lastOption.map(_._1.end)

  def toList: List[(TimestampInterval, TimeAccounting.Context)] =
    toMap.toList

  def isEmpty: Boolean =
    toMap.isEmpty

  def nonEmpty: Boolean =
    toMap.nonEmpty

  def contains(t: Timestamp): Boolean = {
    val i = TimestampInterval.empty(t)
    toMap.maxBefore(i).exists(_._1.contains(t)) ||
      toMap.minAfter(i).exists(_._1.contains(t))
  }

  /**
   * Calculates the time accounting charge associated with this state.
   */
  def charge: CategorizedTime =
    toMap.foldLeft(CategorizedTime.Zero) { case (c, (n, x)) =>
      c.sumCharge(x.chargeClass, n.timeSpan)
    }

  /**
   * Looks up the context at this given `Timestamp`, assuming it is covered by
   * the state.
   */
  def entryAt(t: Timestamp): Option[(TimestampInterval, TimeAccounting.Context)] =
    toMap
      .maxBefore(TimestampInterval.empty(t))
      .filter { case (interval, _) => interval.contains(t) }
      .orElse(
        // if t happens to be the start of an interval, `[t, t)` will
        // sort before it and min "after" will start at `t`.
        toMap
          .minAfter(TimestampInterval.empty(t))
          .filter { case (interval, _) => interval.contains(t) }
      )

  /**
   * Looks up the context at this given `Timestamp`, assuming it is covered by
   * the state.
   */
  def contextAt(t: Timestamp): Option[TimeAccounting.Context] =
    entryAt(t).map(_._2)

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
   * Calculates the set of `Atom.Id` associated with intervals in this state.
   */
  def allAtoms: SortedSet[Atom.Id] =
    toMap.foldLeft(SortedSet.empty[Atom.Id]) { case (atoms, (_, ctx)) =>
      ctx.atom.fold(atoms) { atomContext => atoms + atomContext.atomId }
    }

  /**
   * Returns the set of `Atom.Id` that are associated with state intervals which
   * intersect with `interval`.
   */
  def atomsIntersecting(interval: TimestampInterval): SortedSet[Atom.Id] =
    if (interval.nonEmpty) between(interval).allAtoms
    else SortedSet.from(contextAt(interval.start).flatMap(_.atom).map(_.atomId))

  /**
   * The minimal interval containing all the given atoms, if any.
   */
  def intervalContaining(atoms: Set[Atom.Id]): Option[TimestampInterval] =
    toMap.foldLeft(none[TimestampInterval]) { case (result, (interval, ctx)) =>
      if (!ctx.atom.map(_.atomId).exists(atoms)) result
      else result.fold(interval.some)(_.span(interval).some)
    }

  /**
   * Partitions the state into two, the part included in `interval` and the
   * part excluding `interval`.
   *
   * @return a tuple containing the portion of state that intersects with
   *         interval and the portion that does not
   */
  def partitionOnInterval(interval: TimestampInterval): (TimeAccountingState, TimeAccountingState) =
    (between(interval), excluding(interval))

  /**
   * Partitions the state into two but avoiding the spliting of intervals
   * associated with atoms.  That is, if an interval is associated with an atom
   * and intersects `interval`, it is included in its entirety.  Essentially
   * `interval` is stretched to include any intervals with atoms it intersects.
   *
   * @return a tuple containing the portion of state that intersects with
   *         `interval` (keeping atoms together) and the portion that does
   *         intersect
   */
  def partitionOnAtomBoundary(interval: TimestampInterval): (TimeAccountingState, TimeAccountingState) = {
    val intervalʹ = intervalContaining(atomsIntersecting(interval)).getOrElse(interval).span(interval)
    (between(intervalʹ), excluding(intervalʹ))
  }

  /**
   * Partitions the state into two based on whether interval is associated with
   * the provided `atom`.
   *
   * @return a tuple containing the portion of the state associated with `atom`
   *         and the portion not associated with `atom`
   */
  def partitionOnAtom(atom: Atom.Id): (TimeAccountingState, TimeAccountingState) = {
    val (in, out) = toMap.foldLeft((Empty.toMap, Empty.toMap)) { case ((in, out), (interval, ctx)) =>
      if (ctx.atom.exists(_.atomId === atom)) (in + (interval -> ctx), out)
      else (in, out + (interval -> ctx))
    }
    (new TimeAccountingState(in), new TimeAccountingState(out))
  }

  /**
   * Finds the setups in this state and charges them according to `charged`.
   * This state is expected to be the complete (undiscounted) state of a visit,
   * which determines where the setups are, while `charged` is what remains
   * after discounts are applied, which determines what they cost.
   *
   * A setup is a maximal run of time not spent executing science atoms that
   * includes slewing or executing acquisition atoms.  It ends when the first
   * science step begins.  Setups that include a slew, or which precede any
   * science in the visit, are full setups; those that follow science without
   * slewing are reacquisitions.  Within a setup, time is charged to slewing,
   * to the acquisition sequence, or else to "other" setup time.
   */
  def setups(charged: TimeAccountingState): List[TimeAccounting.Setup] = {

    type Entry = (TimestampInterval, TimeAccounting.Context)

    def isScience(c: TimeAccounting.Context): Boolean =
      c.atom.exists(_.sequenceType === SequenceType.Science)

    def isAcquisition(c: TimeAccounting.Context): Boolean =
      c.atom.exists(_.sequenceType === SequenceType.Acquisition)

    // Maximal runs of non-science entries, each paired with whether any
    // science precedes it in the visit.
    @tailrec
    def runs(
      rem:  List[Entry],
      seen: Boolean,
      acc:  List[(Boolean, NonEmptyList[Entry])]
    ): List[(Boolean, NonEmptyList[Entry])] = {
      val (sci, rest)      = rem.span(e => isScience(e._2))
      val seenʹ            = seen || sci.nonEmpty
      val (run, remaining) = rest.span(e => !isScience(e._2))
      NonEmptyList.fromList(run) match {
        case None      => acc.reverse
        case Some(nel) => runs(remaining, seenʹ, (seenʹ, nel) :: acc)
      }
    }

    def charge(interval: TimestampInterval): SetupTimeBreakdown =
      charged.between(interval).toList.foldLeft(SetupTimeBreakdown.Zero) { case (c, (i, ctx)) =>
        val t = CategorizedTime.Zero.sumCharge(ctx.chargeClass, i.timeSpan)
        val l =
          if (ctx.slewing) SetupTimeBreakdown.slew
          else if (isAcquisition(ctx)) SetupTimeBreakdown.acquisition
          else SetupTimeBreakdown.other
        l.modify(_ |+| t)(c)
      }

    runs(toList, seen = false, Nil).collect {
      case (seen, run) if run.exists((_, c) => c.slewing || isAcquisition(c)) =>
        val kind     = if (run.exists(_._2.slewing) || !seen) SetupKind.Full else SetupKind.Reacquisition
        val interval = run.head._1.span(run.last._1)
        TimeAccounting.Setup(kind, interval, charge(interval))
    }
  }

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
  ): Pipe[F, TimeAccounting.Event, TimeAccountingState] = { in =>

    import TimeAccounting.AtomContext
    import TimeAccounting.Context
    import TimeAccounting.Event

    // Mark the events that begin or continue a period of slewing.  A slew
    // starts with a slew start event and lasts until the slew end or the next
    // atom event, whichever comes first.
    val markSlewing: Pipe[F, Event, Event] =
      _.mapAccumulate(false) { (slewing, e) =>
         val slewingʹ = e.context.atom.fold(e.slewStage.fold(slewing)(_ === SlewStage.StartSlew))(_ => false)
         (slewingʹ, Event.context.andThen(Context.slewing).replace(slewingʹ)(e))
       }
       .map(_._2)

    // Clean the event stream to make it easier to process.
    // * We don't need sequence level events, except at the beginning and end if
    //   present there or where slewing starts or stops
    // * Contiguous events with the same context can be collapsed into just the
    //   first and last ones
    val cleanEvents: Pipe[F, Event, Event] =
      _.zipWithPreviousAndNext
       .flatMap {
         case (None,       cur, _   ) => Stream.emit(cur)
         case (_,          cur, None) => Stream.emit(cur)
         case (Some(prev), cur, _   ) => if (cur.context.atom.isDefined || cur.context.slewing =!= prev.context.slewing) Stream.emit(cur)
                                         else Stream.empty
       }
       .groupAdjacentBy(_.context)
       .flatMap { case (ctx, events) =>
         if (events.size <= 1) Stream.fromOption(events.head)
         else Stream.emits((events.head, events.last).mapN((s, e) => List(s,e)).toList.flatten)
       }

    val makeEntries: Pipe[F, Event, (TimestampInterval, Context)] =
      _.zipWithNext
       .flatMap {
         case (cur, Some(next)) if cur.timestamp > next.timestamp =>
           throw new RuntimeException("Events out of order!")

         case (cur, Some(next)) if cur.context === next.context =>
           Stream.emit(
             cur.timestamp.intervalUntil(next.timestamp) -> cur.context
           )

         case (cur, Some(next)) if cur.context.atom.isDefined && cur.context.atom.map(_.atomId) === next.context.atom.map(_.atomId) =>
           // Time within an atom, but not in any particular step.
           val ac  = cur.context.atom.map(AtomContext.stepId.replace(None))
           val ctx = Context(visitId, chargeClass, ac, slewing = false)
           Stream.emit(
             cur.timestamp.intervalUntil(next.timestamp) -> ctx
           )

         case (cur, Some(next)) =>
           // Time not in any particular atom, possibly slewing.
           Stream.emit(
             cur.timestamp.intervalUntil(next.timestamp) -> Context(visitId, chargeClass, None, cur.context.slewing)
           )

         case (cur, None) =>
           // Last event has already been taken into account.
           Stream.empty
       }


    in.through(markSlewing)
     .through(cleanEvents)
     .through(makeEntries)
     .filter { case (interval, _) => interval.nonEmpty }
     .fold(Empty.toMap) { case (state, (interval, ctx)) =>
       state + (interval -> ctx)
     }
     .map(new TimeAccountingState(_))

  }
}
