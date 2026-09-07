// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.enums.TooActivation
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.unionAll
import lucuma.core.model.Observation
import lucuma.core.model.SchedulingAvailability
import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowEnd
import lucuma.core.model.TimingWindowRepeat
import lucuma.core.syntax.time.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.*
import org.typelevel.cats.time.given
import skunk.*
import skunk.codec.numeric.int4
import skunk.codec.temporal.date
import skunk.implicits.*
import spire.math.extras.interval.IntervalSeq

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

import Services.Syntax.*

/**
 * An observation's *scheduling availability*: the total time it is available
 * for scheduling.  Science staff call this the scheduling window and mean this
 * sum, never the length of any single opening, and it is about how long the
 * observation is *open* -- nothing to do with how long it takes to execute.
 *
 * Wall clock rather than observable time -- the time the target is actually up
 * and observable -- because working that out needs to know where the target is
 * and when the window falls, and a Target of Opportunity awaiting its alert has
 * neither: its target is unresolved, and its window has no start until the
 * trigger supplies one.  Since that is exactly the case this measure exists to
 * police, wall clock is the only measure available for all of them.
 *
 * The value is recorded on a configuration request when the request is made,
 * and an observation stays covered by that request while its availability is at
 * least as much as the one recorded.
 */
trait SchedulingAvailabilityService[F[_]]:

  /**
   * The scheduling availability of each observation.  Every id naming an
   * existing observation appears in the result.
   */
  def select(
    oids: List[Observation.Id]
  ): F[Map[Observation.Id, SchedulingAvailability]]

object SchedulingAvailabilityService:

  /**
   * The time within `measured` covered by the INCLUDE windows and not by the
   * EXCLUDE ones.  With no windows at all the observation is open for the whole
   * of it, and with no INCLUDE windows the EXCLUDE ones cut into that whole.
   */
  def fromTimingWindows(
    windows:  List[TimingWindow],
    measured: BoundedInterval[Instant]
  ): TimeSpan =
    if windows.isEmpty then TimeSpan.unsafeFromDuration(measured.duration)
    else
      val (exclude, include) = windows.partition(_.inclusion === TimingWindowInclusion.Exclude)
      val base      = if include.isEmpty then IntervalSeq(measured)
                      else IntervalSeq.unionAll(include.map(_.toIntervalSeq(measured)))
      val available = base & ~IntervalSeq.unionAll(exclude.map(_.toIntervalSeq(measured)))
      TimeSpan.fromDuration(available.duration).getOrElse(TimeSpan.Max)

  /**
   * The time a Target of Opportunity stated it needs once triggered, which is
   * not clipped by the active period: the length says how long the observation
   * needs after the alert, not where in the semester that falls.  Saying nothing
   * means the whole of what remains.
   *
   * Saying nothing is deliberately not the activation's default window.  That
   * default is the database's, not the PI's, and what it follows from -- how
   * disruptive the ToO is -- is already governed by the proposal's activation
   * ceiling.  Recording it here would police the same fact twice, so that merely
   * escalating a standard ToO to rapid would read as the PI shortening their own
   * window.
   */
  def fromTooWindow(
    stated:   Option[TimeSpan],
    measured: BoundedInterval[Instant]
  ): TimeSpan =
    stated.getOrElse(TimeSpan.unsafeFromDuration(measured.duration))

  /**
   * How available the observation is, measured over the stretch described by
   * [[measurementInterval]].
   *
   * An observation declared after the active period has run out has nothing left
   * to offer and is asked for nothing; whether it can still be executed is a
   * different question from whether it is approved.
   */
  def availability(
    anchor:     Timestamp,
    activeFrom: LocalDate,
    activeTo:   LocalDate,
    activation: TooActivation,
    tooWindow:  Option[TimeSpan],
    windows:    List[TimingWindow]
  ): SchedulingAvailability =
    measurementInterval(anchor, activeFrom, activeTo).fold(SchedulingAvailability.Zero): measured =>
      val remaining = TimeSpan.unsafeFromDuration(measured.duration)
      // A ToO's window is what it stated, not what its timing windows happen to
      // say: once triggered it *has* a materialized window, and measuring that
      // would make every ToO triggered late in the semester look like it had
      // shortened its own window.
      val open =
        if activation =!= TooActivation.None then fromTooWindow(tooWindow, measured)
        else fromTimingWindows(windows, measured)
      SchedulingAvailability(open, remaining)

  /**
   * The stretch the window is measured over: from the later of the anchor and
   * the start of the active period, to its end.  None when that leaves nothing.
   *
   * Taking the later of the two matters for the ordinary case, where an
   * observation is designed before its semester opens: that one is measured over
   * the whole active period, not penalised for having been thought of early.
   * Only a declaration made once the period is under way is measured from itself.
   */
  def measurementInterval(
    anchor:     Timestamp,
    activeFrom: LocalDate,
    activeTo:   LocalDate
  ): Option[BoundedInterval[Instant]] =
    val from = anchor.toInstant.max(activeFrom.atStartOfDay(ZoneOffset.UTC).toInstant)
    val to   = activeTo.atStartOfDay(ZoneOffset.UTC).toInstant
    Option.when(from.isBefore(to))(BoundedInterval.unsafeOpenUpper(from, to))

  def instantiate[F[_]: Concurrent](using Services[F]): SchedulingAvailabilityService[F] =
    new SchedulingAvailabilityService[F]:

      override def select(
        oids: List[Observation.Id]
      ): F[Map[Observation.Id, SchedulingAvailability]] =
        NonEmptyList.fromList(oids).fold(Map.empty[Observation.Id, SchedulingAvailability].pure[F]): nel =>
          val enc = observation_id.nel(nel)
          session
            .stream(Statements.SelectWindows(enc))(nel, 1024)
            .compile
            .toList
            .map: rows =>
              rows
                .groupMap(_._1)(r => (r._2, r._3, r._4, r._5, r._6, r._7))
                .map:
                  case (oid, values) =>
                    val (anchor, from, to, activation, tooWindow, _) = values.head
                    oid -> availability(anchor, from, to, activation, tooWindow, values.flatMap(_._6))

  object Statements:

    private val timing_window: Decoder[Option[TimingWindow]] =
      (
        timing_window_inclusion.opt *:
        core_timestamp.opt          *:
        core_timestamp.opt          *:
        time_span.opt               *:
        time_span.opt               *:
        int4.opt
      ).map:
        case (Some(inclusion), Some(start), endAt, endAfter, period, times) =>
          val end =
            endAt
              .map(TimingWindowEnd.At(_))
              .orElse:
                endAfter.map: d =>
                  TimingWindowEnd.After(d, period.map(p => TimingWindowRepeat(p, times.flatMap(PosInt.from(_).toOption))))
          TimingWindow(inclusion, start, end).some
        case _ =>
          none

    // One row per timing window, or a single row with a null window for an
    // observation that has none, so that every observation is represented.
    def SelectWindows[A <: NonEmptyList[Observation.Id]](
      enc: Encoder[A]
    ): Query[A, (Observation.Id, Timestamp, LocalDate, LocalDate, TooActivation, Option[TimeSpan], Option[TimingWindow])] =
      sql"""
        SELECT
          o.c_observation_id,
          o.c_availability_anchor,
          p.c_active_start,
          p.c_active_end,
          o.c_too_activation,
          o.c_too_window,
          w.c_inclusion,
          w.c_start,
          w.c_end_at,
          w.c_end_after,
          w.c_repeat_period,
          w.c_repeat_times
        FROM t_observation o
        JOIN t_program p ON p.c_program_id = o.c_program_id
        LEFT JOIN t_timing_window w ON w.c_observation_id = o.c_observation_id
        WHERE o.c_observation_id IN ($enc)
      """
      .query(observation_id *: core_timestamp *: date *: date *: too_activation *: time_span.opt *: timing_window)
