// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.TimingWindowInclusion.Include
import lucuma.core.model.Observation
import lucuma.core.util.Timestamp
import lucuma.odb.util.Codecs.observation_id
import skunk.codec.boolean.bool
import skunk.implicits.*

import java.time.Duration
import java.time.temporal.ChronoUnit

/**
 * The window a trigger brings with it.  A rapid or interrupting ToO that says
 * nothing about its own timing is given 24 hours to run in -- and gives them
 * back, since the window belongs to the request rather than to the observation.
 */
class tooTriggerDefaultWindow extends ExecutionTestSupportForGmos with TooTriggerSetupOperations:

  private val OneDaySeconds: Long = 24L * 60L * 60L

  private def windows(oid: Observation.Id): IO[List[Window]] =
    getTimingWindowsAs(pi, oid)

  private def setWorkflowState(oid: Observation.Id, s: ObservationWorkflowState): IO[Unit] =
    setTooWorkflowState(pi, oid, s)

  private def requestedAt(oid: Observation.Id): IO[Timestamp] =
    getRequestedTooTriggerAs(pi, oid).map(_.requestedAt)

  /** The window the default rule should have produced for a request made at `at`. */
  private def defaultWindow(at: Timestamp): Window =
    Window(Include, at, at.plusSecondsOption(OneDaySeconds))

  /** Whether each of the observation's windows is automatic, which no query exposes. */
  private def automatic(oid: Observation.Id): IO[List[Boolean]] =
    withSession: session =>
      session.execute(
        sql"""
          SELECT c_automatic
            FROM t_timing_window
           WHERE c_observation_id = $observation_id
           ORDER BY c_timing_window_id
        """.query(bool)
      )(oid)

  /** `days` from now, as a Timestamp and as the string a mutation wants. */
  private def fromNow(days: Long): IO[(Timestamp, String)] =
    IO.realTimeInstant.map: now =>
      val t = Timestamp.unsafeFromInstantTruncated(now.plus(Duration.ofDays(days)).truncatedTo(ChronoUnit.SECONDS))
      (t, t.isoFormat)

  private def beginExecution(oid: Observation.Id): IO[Unit] =
    for
      vid <- recordVisitAs(serviceUser, oid)
      _   <- addSequenceEventAs(serviceUser, vid, SequenceCommand.Start)
    yield ()

  // WHICH TRIGGERS GET ONE ----------------------------------------------------

  test("a rapid trigger with no timing windows opens a 24 hour window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      before      <- windows(oid)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      at          <- requestedAt(oid)
      after       <- windows(oid)
    yield
      assertEquals(before, Nil)
      // Anchored to the request itself rather than to some later moment: the
      // window starts at the trigger's own requestedAt.
      assertEquals(after, List(defaultWindow(at)))

  test("an interrupting trigger opens a 24 hour window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Interrupting)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      at          <- requestedAt(oid)
      after       <- windows(oid)
    yield assertEquals(after, List(defaultWindow(at)))

  // A standard ToO waits its turn in the queue like anything else, so a 24 hour
  // deadline describes nothing about it.
  test("a standard trigger opens no window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Unconstrained)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      after       <- windows(oid)
    yield assertEquals(after, Nil)

  test("a non-ToO observation set Ready gets no window"):
    for
      (_, oid) <- createTriggerableObservationAs(pi, staff)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      ws       <- windows(oid)
    yield assertEquals(ws, Nil)

  test("the window a trigger opens is marked automatic"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      flags       <- automatic(oid)
    yield assertEquals(flags, List(true))

  // WHAT IT LEAVES ALONE ------------------------------------------------------

  test("an observation with a window of its own gets no default"):
    for
      (_, oid, _)     <- createTooObservationAs(pi, staff)
      (start, startS) <- fromNow(-1)
      _               <- setTimingWindowsAs(pi, oid, s"""[ { inclusion: INCLUDE, startUtc: "$startS" } ]""")
      _               <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      after           <- windows(oid)
      flags           <- automatic(oid)
    yield
      assertEquals(after, List(Window(Include, start, none)))
      // The PI's, so nothing here will ever remove it.
      assertEquals(flags, List(false))

  // The rule asks whether the PI said anything, not whether what they said is
  // still useful.  A window they wrote is theirs even once it has closed, and
  // second-guessing it would mean overriding an explicit answer.
  test("a window of the PI's own is respected even after it has closed"):
    for
      (_, oid, _)     <- createTooObservationAs(pi, staff)
      (start, startS) <- fromNow(-10)
      (end, endS)     <- fromNow(-9)
      _               <- setTimingWindowsAs(pi, oid, s"""[ { inclusion: INCLUDE, startUtc: "$startS", end: { atUtc: "$endS" } } ]""")
      _               <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      after           <- windows(oid)
    yield assertEquals(after, List(Window(Include, start, end.some)))

  // TAKING IT BACK ------------------------------------------------------------

  test("withdrawing the trigger removes the window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      during      <- windows(oid)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Defined)
      after       <- windows(oid)
    yield
      assertEquals(during.length, 1)
      assertEquals(after, Nil)

  test("declining the trigger removes the window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      rid         <- getRequestedTooTriggerAs(pi, oid).map(_.id)
      _           <- declineTooTrigger(staff, rid, "weathered out".some)
      after       <- windows(oid)
    yield assertEquals(after, Nil)

  // Acceptance is not a revert.  Taking the window back from an observation the
  // observatory has started would hand it an unbounded schedule, which is the
  // opposite of what the window was for.
  test("acceptance keeps the window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      at          <- requestedAt(oid)
      _           <- beginExecution(oid)
      after       <- windows(oid)
    yield assertEquals(after, List(defaultWindow(at)))

  test("withdrawing does not touch a window the PI wrote"):
    for
      (_, oid, _)     <- createTooObservationAs(pi, staff)
      (start, startS) <- fromNow(-1)
      _               <- setTimingWindowsAs(pi, oid, s"""[ { inclusion: INCLUDE, startUtc: "$startS" } ]""")
      _               <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _               <- setWorkflowState(oid, ObservationWorkflowState.Defined)
      after           <- windows(oid)
    yield assertEquals(after, List(Window(Include, start, none)))

  // The case the ownership exists for: without it the second request would find
  // the first one's window still sitting there, add nothing, and inherit a
  // deadline that expired while nobody was asking.
  test("re-triggering after a withdrawal opens a fresh window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      first       <- windows(oid)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Defined)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      at          <- requestedAt(oid)
      second      <- windows(oid)
    yield
      assertEquals(second, List(defaultWindow(at)))
      assertNotEquals(second, first)

  // Supersession closes one request and opens another, so the window follows:
  // the successor is a different request, made at a different time, and its 24
  // hours run from its own.
  test("superseding a request replaces the window"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      before      <- windows(oid)
      _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Interrupting)
      at          <- requestedAt(oid)
      after       <- windows(oid)
    yield
      assertEquals(before.length, 1)
      assertEquals(after, List(defaultWindow(at)))
      assertNotEquals(after, before)

  // Superseded downwards, though, the successor is standard and wants no window
  // at all -- so the close-out takes the rapid request's window and nothing
  // replaces it.
  test("superseding down off the ladder removes the window")  :
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      before      <- windows(oid)
      _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Unconstrained)
      after       <- windows(oid)
    yield
      assertEquals(before.length, 1)
      assertEquals(after, Nil)

  // ... and the reverse: a standard request escalated onto the ladder reaches
  // the rule for the first time.
  test("escalating a standard request onto the ladder opens a window"):
    for
      // Created uninterruptible so the frozen ceiling admits the escalation,
      // then lowered before the request is made.
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Unconstrained)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      standard    <- windows(oid)
      _           <- setSchedulingModeAs(pi, oid, SchedulingMode.Uninterruptible)
      at          <- requestedAt(oid)
      after       <- windows(oid)
    yield
      assertEquals(standard, Nil)
      assertEquals(after, List(defaultWindow(at)))
