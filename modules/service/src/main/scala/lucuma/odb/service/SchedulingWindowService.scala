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
import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowEnd
import lucuma.core.model.TimingWindowRepeat
import lucuma.core.syntax.time.*
import lucuma.core.util.TimeSpan
import lucuma.odb.data.TooWindow
import lucuma.odb.util.Codecs.*
import org.typelevel.cats.time.given
import skunk.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int4
import skunk.codec.temporal.date
import skunk.implicits.*
import spire.math.extras.interval.IntervalSeq

import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset

import Services.Syntax.*

/**
 * An observation's *scheduling window*: the total time it is available for
 * scheduling.  Science staff say "the window" and mean this sum, never the
 * length of any single opening, and it is about how long the observation is
 * *open* -- nothing to do with how long it takes to execute.
 *
 * Wall clock rather than observable time, deliberately: a Target of Opportunity
 * still waiting on its alert has no coordinates at all, so observable time is
 * undefined for exactly the observations this measure is meant to police.
 *
 * The value is recorded on a configuration request when the request is made,
 * and an observation stays covered by that request while its window is at least
 * as long as the one recorded.
 */
trait SchedulingWindowService[F[_]]:

  /**
   * The scheduling window of each observation.  Every id naming an existing
   * observation appears in the result.
   */
  def select(
    oids: List[Observation.Id]
  ): F[Map[Observation.Id, TimeSpan]]

object SchedulingWindowService:

  /**
   * The scheduling window of an ordinary observation: the time within `active`
   * covered by its INCLUDE windows and not by its EXCLUDE windows.  With no
   * windows at all the observation is available for the whole active period,
   * and with no INCLUDE windows the EXCLUDE ones cut into that whole.
   */
  def fromTimingWindows(
    windows: List[TimingWindow],
    active:  BoundedInterval[Instant]
  ): TimeSpan =
    if windows.isEmpty then TimeSpan.unsafeFromDuration(active.duration)
    else
      val (exclude, include) = windows.partition(_.inclusion === TimingWindowInclusion.Exclude)
      val base      = if include.isEmpty then IntervalSeq(active)
                      else IntervalSeq.unionAll(include.map(_.toIntervalSeq(active)))
      val available = base & ~IntervalSeq.unionAll(exclude.map(_.toIntervalSeq(active)))
      TimeSpan.fromDuration(available.duration).getOrElse(TimeSpan.Max)

  /**
   * The scheduling window of a Target of Opportunity: the length it stated, not
   * clipped by the active period, since the length says how long the observation
   * needs once triggered rather than where in the semester that falls.  Forever
   * is the whole active period, and so is saying nothing.
   *
   * Saying nothing is deliberately not the activation's default window.  That
   * default is the database's, not the PI's, and what it follows from -- how
   * disruptive the ToO is -- is already governed by the proposal's activation
   * ceiling.  Recording it here would police the same fact twice, so that merely
   * escalating a standard ToO to rapid would read as the PI shortening their own
   * window.
   */
  def fromTooWindow(
    stated: Option[TooWindow],
    active: BoundedInterval[Instant]
  ): TimeSpan =
    stated match
      case Some(TooWindow.For(d)) => d
      case _                      => TimeSpan.unsafeFromDuration(active.duration)

  def schedulingWindow(
    activation: TooActivation,
    tooWindow:  Option[TooWindow],
    windows:    List[TimingWindow],
    active:     BoundedInterval[Instant]
  ): TimeSpan =
    // A ToO's window is what it stated, not what its timing windows happen to
    // say: once triggered it *has* a materialized window, and measuring that
    // would make every ToO triggered late in the semester look like it had
    // shortened its own window.
    if activation =!= TooActivation.None then fromTooWindow(tooWindow, active)
    else fromTimingWindows(windows, active)

  /** The program's active period as an instant interval, `[start, end)` at UTC midnight. */
  def activePeriod(start: LocalDate, end: LocalDate): BoundedInterval[Instant] =
    BoundedInterval.unsafeOpenUpper(
      start.atStartOfDay(ZoneOffset.UTC).toInstant,
      end.atStartOfDay(ZoneOffset.UTC).toInstant
    )

  def instantiate[F[_]: Concurrent](using Services[F]): SchedulingWindowService[F] =
    new SchedulingWindowService[F]:

      override def select(
        oids: List[Observation.Id]
      ): F[Map[Observation.Id, TimeSpan]] =
        NonEmptyList.fromList(oids).fold(Map.empty[Observation.Id, TimeSpan].pure[F]): nel =>
          val enc = observation_id.nel(nel)
          session
            .stream(Statements.SelectWindows(enc))(nel, 1024)
            .compile
            .toList
            .map: rows =>
              rows
                .groupMap(_._1)(r => (r._2, r._3, r._4, r._5))
                .map:
                  case (oid, values) =>
                    val (active, activation, tooWindow, _) = values.head
                    val tws = values.flatMap(_._4)
                    oid -> schedulingWindow(activation, tooWindow, tws, active)

  object Statements:

    private val too_window: Decoder[Option[TooWindow]] =
      (time_span.opt *: bool).map:
        case (Some(d), _) => TooWindow.For(d).some
        case (_, true)    => TooWindow.Forever.some
        case _            => none

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
    ): Query[A, (Observation.Id, BoundedInterval[Instant], TooActivation, Option[TooWindow], Option[TimingWindow])] =
      sql"""
        SELECT
          o.c_observation_id,
          p.c_active_start,
          p.c_active_end,
          o.c_too_activation,
          o.c_too_window,
          o.c_too_window_forever,
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
      .query(observation_id *: date *: date *: too_activation *: too_window *: timing_window)
      .map:
        case (oid, start, end, activation, tooWindow, window) =>
          (oid, activePeriod(start, end), activation, tooWindow, window)
