// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.Partner
import lucuma.core.enums.SchedulingMode
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.util.Codecs.*
import skunk.codec.numeric.int8
import skunk.syntax.all.*

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

/**
 * Scheduling availability -- how open an observation is, as opposed to how long
 * it takes to execute -- is recorded on a configuration request and enforced
 * against the observation thereafter.
 *
 * Everything here is relative to the present, because the measure is: it runs
 * from the later of the declaration and the start of the active period, to the
 * end of that period.  A fixture whose semester sat wholly in the past would make
 * every measurement zero and every assertion vacuous.
 */
class schedulingAvailabilityApproval extends OdbSuite with ObservingModeSetupOperations:

  val admin: User = TestUsers.Standard.admin(3, 32)
  val pi: User    = TestUsers.Standard.pi(1, 30)
  val service     = TestUsers.service(4)

  val validUsers: List[User] = List(pi, admin, service)

  private val Now: Instant = Instant.now()

  // A semester a month old with five months still to run.
  private val ActiveStart: LocalDate = LocalDate.ofInstant(Now, ZoneOffset.UTC).minusDays(30)
  private val ActiveEnd: LocalDate   = LocalDate.ofInstant(Now, ZoneOffset.UTC).plusDays(150)

  /** Hours between the present and the end of the active period. */
  private val HoursRemaining: BigDecimal =
    BigDecimal(Duration.between(Now, ActiveEnd.atStartOfDay(ZoneOffset.UTC).toInstant).toMinutes) / 60

  private def utc(i: Instant): String =
    DateTimeFormatter.ISO_INSTANT.format(i.truncatedTo(ChronoUnit.SECONDS))

  private def window(fromNow: Long, hours: Long): String =
    s"""[{ inclusion: INCLUDE, startUtc: "${utc(Now.plus(Duration.ofHours(fromNow)))}", end: { after: { hours: $hours } } }]"""

  /** The cheat: opens back at the start of the semester, shuts `hours` from now. */
  private def windowOpeningInThePast(hours: Long): String =
    s"""[{
          inclusion: INCLUDE,
          startUtc: "${utc(ActiveStart.atStartOfDay(ZoneOffset.UTC).toInstant)}",
          end: { atUtc: "${utc(Now.plus(Duration.ofHours(hours)))}" }
        }]"""

  private def setup: IO[(Program.Id, Target.Id)] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin, activeStart = ActiveStart, activeEnd = ActiveEnd)
      pid   <- createProgramAs(pi, "Scheduling availability approval")
      _     <- addProposal(pi, pid, cfpid.some, None)
      tid   <- createTargetWithProfileAs(pi, pid)
    yield (pid, tid)

  private def setTimingWindows(oid: Observation.Id, windows: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { timingWindows: $windows } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def setTooWindow(oid: Observation.Id, w: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { tooWindow: $w } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Winds an observation's anchor back, standing in for having declared it earlier. */
  private def declareDaysAgo(oid: Observation.Id, days: Long): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_observation
             SET c_availability_anchor = now() - ($int8 * INTERVAL '1 day')
           WHERE c_observation_id = $observation_id
        """.command
      )(days, oid).void

  /** The requests that still cover this observation; empty means it needs a new one. */
  private def coveringRequests(oid: Observation.Id): IO[List[ConfigurationRequest.Id]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            configurationRequests { id }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("observation", "configurationRequests")
       .values
       .toList
       .flatten
       .flatMap(_.hcursor.downField("id").as[ConfigurationRequest.Id].toOption)

  /** (timeOpen, timeRemainingWhenDeclared), in hours. */
  private def availability(oid: Observation.Id): IO[(BigDecimal, BigDecimal)] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            configuration {
              availability {
                timeOpen { hours }
                timeRemainingWhenDeclared { hours }
              }
            }
          }
        }
      """
    ).map: js =>
      val c = js.hcursor.downFields("observation", "configuration", "availability")
      (
        c.downFields("timeOpen", "hours").require[BigDecimal],
        c.downFields("timeRemainingWhenDeclared", "hours").require[BigDecimal]
      )

  private def timeOpen(oid: Observation.Id): IO[BigDecimal] =
    availability(oid).map(_._1)

  /** The stated ToO window in hours, or None when nothing was stated. */
  private def tooWindowJson(oid: Observation.Id): IO[Option[BigDecimal]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            schedulingConstraints { tooWindow { hours } }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("observation", "schedulingConstraints", "tooWindow", "hours")
       .as[BigDecimal]
       .toOption

  /** Within a few minutes, since the fixture's clock and the database's differ slightly. */
  private def assertHours(actual: BigDecimal, expected: BigDecimal, clue: String): Unit =
    assert((actual - expected).abs < BigDecimal("0.1"), s"$clue: expected ~$expected hours, got $actual")

  // -- The measure -----------------------------------------------------------

  test("an observation with no timing windows withholds nothing"):
    for
      (pid, tid)  <- setup
      oid         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      (open, rem) <- availability(oid)
    yield
      assertHours(rem, HoursRemaining, "time remaining")
      assertEquals(open, rem, "Saying nothing offers everything that is left.")

  test("the measure runs from the declaration, not from the start of the semester"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      (_, rem)   <- availability(oid)
    yield
      // The semester is 180 days long but only 150 of them are still to come.
      assertHours(rem, HoursRemaining, "time remaining")
      assert(rem < BigDecimal(180 * 24), s"Elapsed days must not be counted, got $rem")

  test("an observation designed before the semester is measured over the whole of it"):
    for
      (pid, tid)  <- setup
      oid         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      // Designed a fortnight before the call opened, which is the normal case.
      _           <- declareDaysAgo(oid, 44)
      (open, rem) <- availability(oid)
    yield
      // Measurement starts at the active period, not at the declaration, so it
      // gets the full 180 days rather than a bar raised by its own early start.
      assertHours(rem, BigDecimal(180 * 24), "time remaining")
      assertEquals(open, rem)

  test("timing windows narrow it to their total"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      open       <- timeOpen(oid)
    yield assertHours(open, BigDecimal(6), "time open")

  // -- The cheat -------------------------------------------------------------

  test("a window opening before it was declared cannot borrow the elapsed time"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, windowOpeningInThePast(hours = 4))
      open       <- timeOpen(oid)
    yield
      // It nominally spans a month, but a month of it has already gone.
      assertHours(open, BigDecimal(4), "time open")

  test("a window opening in the past is not covered by a wide approved one"):
    for
      (pid, tid) <- setup
      approved   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- createConfigurationRequestAs(pi, approved)
      sneaky     <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(sneaky, windowOpeningInThePast(hours = 4))
      covered    <- coveringRequests(sneaky)
    yield assertEquals(covered, Nil, "Four usable hours is not a semester's availability.")

  // -- The cap ---------------------------------------------------------------

  test("an observation added later, open for all that remains, stays covered"):
    for
      (pid, tid) <- setup
      // Approved a hundred days ago, when far more of the semester was left.
      approved   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- declareDaysAgo(approved, 100)
      rid        <- createConfigurationRequestAs(pi, approved)
      // Added today, withholding nothing -- but it cannot offer the hundred days
      // it never had.
      late       <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      covered    <- coveringRequests(late)
    yield assertEquals(covered, List(rid), "It offers everything it has; that must be enough.")

  test("stating your availability is not worse than saying nothing"):
    for
      (pid, tid) <- setup
      approved   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- declareDaysAgo(approved, 100)
      rid        <- createConfigurationRequestAs(pi, approved)
      silent     <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      stated     <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(stated, window(fromNow = 0, hours = 150 * 24))
      a          <- coveringRequests(silent)
      b          <- coveringRequests(stated)
    yield
      assertEquals(a, List(rid))
      assertEquals(b, List(rid), "Honesty must not cost approval.")

  // -- Enforcement -----------------------------------------------------------

  test("adding a window to an observation approved without one needs a new request"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      rid        <- createConfigurationRequestAs(pi, oid)
      before     <- coveringRequests(oid)
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      after      <- coveringRequests(oid)
    yield
      assertEquals(before, List(rid))
      assertEquals(after, Nil, "A window where there were none is a shortening.")

  test("widening an approved window keeps the observation covered"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      rid        <- createConfigurationRequestAs(pi, oid)
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 12))
      after      <- coveringRequests(oid)
    yield assertEquals(after, List(rid), "Widening needs no approval.")

  test("shortening an approved window needs a new request"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 12))
      _          <- createConfigurationRequestAs(pi, oid)
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      after      <- coveringRequests(oid)
    yield assertEquals(after, Nil)

  test("removing the windows entirely keeps the observation covered"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      rid        <- createConfigurationRequestAs(pi, oid)
      _          <- setTimingWindows(oid, "[]")
      after      <- coveringRequests(oid)
    yield assertEquals(after, List(rid))

  test("two requests differing only in the availability are distinct"):
    for
      (pid, tid) <- setup
      o1         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(o1, window(fromNow = 1, hours = 6))
      r1         <- createConfigurationRequestAs(pi, o1)
      o2         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(o2, window(fromNow = 1, hours = 12))
      r2         <- createConfigurationRequestAs(pi, o2)
    yield assertNotEquals(r1, r2, s"Expected distinct requests, got $r1 twice.")

  // -- The anchor ------------------------------------------------------------

  test("re-saving the same windows does not move the anchor"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      _          <- declareDaysAgo(oid, 20)
      before     <- availability(oid)
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      after      <- availability(oid)
    yield assertEquals(after, before, "An identical save must not restart the measurement.")

  test("changing the windows does move the anchor"):
    for
      (pid, tid)  <- setup
      oid         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _           <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      _           <- declareDaysAgo(oid, 20)
      (_, before) <- availability(oid)
      _           <- setTimingWindows(oid, window(fromNow = 1, hours = 12))
      (_, after)  <- availability(oid)
    yield assert(after < before, s"Expected the measurement to restart from today, got $after vs $before")

  // -- The ToO window --------------------------------------------------------

  test("the ToO window round-trips"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      unset      <- tooWindowJson(oid)
      _          <- setTooWindow(oid, "{ hours: 6 }")
      stated     <- tooWindowJson(oid)
      _          <- setTooWindow(oid, "null")
      cleared    <- tooWindowJson(oid)
    yield
      assertEquals(unset, None)
      assertEquals(stated, BigDecimal(6).some)
      assertEquals(cleared, None)

  test("clearing the scheduling constraints clears the ToO window"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTooWindow(oid, "{ hours: 6 }")
      stated     <- tooWindowJson(oid)
      _          <- query(
                      pi,
                      s"""
                        mutation {
                          updateObservations(input: {
                            SET: { schedulingConstraints: null }
                            WHERE: { id: { EQ: ${oid.asJson} } }
                          }) {
                            observations { id }
                          }
                        }
                      """
                    ).void
      cleared    <- tooWindowJson(oid)
    yield
      assertEquals(stated, BigDecimal(6).some)
      assertEquals(cleared, None)

  test("an edit that does not mention the scheduling constraints leaves the ToO window alone"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTooWindow(oid, "{ hours: 6 }")
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 3))
      after      <- tooWindowJson(oid)
    yield assertEquals(after, BigDecimal(6).some)

  test("a ToO is measured by its ToO window, not by its timing windows"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      ttid       <- createOpportunityTargetAs(pi, pid)
      _          <- addTargetToAsterism(oid, ttid)
      _          <- setSchedulingMode(oid, SchedulingMode.Uninterruptible)
      // Unstated, a ToO makes no scheduling-window demand of its own: how
      // disruptive it may be is the activation ceiling's business.
      unstated   <- timeOpen(oid)
      _          <- setTooWindow(oid, "{ hours: 6 }")
      stated     <- timeOpen(oid)
      // A timing window is beside the point for a ToO: the trigger supplies one.
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 1))
      withWindow <- timeOpen(oid)
    yield
      assertHours(unstated, HoursRemaining, "unstated")
      assertHours(stated, BigDecimal(6), "stated")
      assertHours(withWindow, BigDecimal(6), "with a timing window")

  // -- Saying why ------------------------------------------------------------
  //
  // An observation that nothing subsumes is reported as needing approval, which
  // is true whichever dimension moved.  Availability is the one a PI can trip
  // without touching the science, so it gets named.

  test("shortening the window says so, with the bar and what was offered"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- computeItcResultAs(pi, oid)
      _          <- createConfigurationRequestAs(pi, oid)
      _          <- accept(pid)
      _          <- setTimingWindows(oid, window(fromNow = 1, hours = 6))
      _          <- runObscalcUpdateAs(service, pid, oid)
      msgs       <- validationMessages(oid)
    yield
      assert(
        msgs.exists(_.startsWith("Less available than approved")),
        s"Expected the availability to be named, got: $msgs"
      )
      assert(
        msgs.exists(_.contains("open for 6 hours")),
        s"Expected what the observation offers, got: $msgs"
      )
      // The bar is the capped one -- everything the observation still had when
      // it was re-declared -- not the whole semester that was approved.
      assert(
        msgs.exists(_.contains("149 days")),
        s"Expected the bar it actually had to clear, got: $msgs"
      )

  test("a change to something else stays generic"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- computeItcResultAs(pi, oid)
      _          <- createConfigurationRequestAs(pi, oid)
      _          <- accept(pid)
      // Better conditions than were approved, with the windows untouched.
      _          <- requireBetterConditions(oid)
      _          <- runObscalcUpdateAs(service, pid, oid)
      msgs       <- validationMessages(oid)
    yield
      assert(msgs.nonEmpty, "Expected the observation to need approval.")
      assert(
        !msgs.exists(_.startsWith("Less available than approved")),
        s"Availability did not move; it must not be blamed. Got: $msgs"
      )

  /** Acceptance is what turns the configuration check on. */
  private def accept(pid: Program.Id): IO[Unit] =
    for
      mid <- piProgramUserIdAs(pi, pid)
      _   <- updateProgramUserAs(pi, mid, PartnerLink.HasNonPartner)
      _   <- addSubmissionPrerequisitesAs(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 100)))
      _   <- submitProposal(pi, pid)
      _   <- acceptProposal(admin, pid)
    yield ()

  private def requireBetterConditions(oid: Observation.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { constraintSet: { imageQuality: POINT_ONE } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Every workflow validation message on an observation. */
  private def validationMessages(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { validationErrors { messages } } }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("observation", "workflow", "value", "validationErrors")
       .values
       .toList
       .flatten
       .flatMap(_.hcursor.downField("messages").values.toList.flatten.flatMap(_.asString))

  private def addTargetToAsterism(oid: Observation.Id, tid: Target.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateAsterisms(input: {
            SET: { ADD: [ ${tid.asJson} ] }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def setSchedulingMode(oid: Observation.Id, mode: SchedulingMode): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { schedulingMode: ${mode.tag.toUpperCase} } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void
