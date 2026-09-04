// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.SchedulingMode
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * The scheduling window -- the total time an observation is available, as
 * opposed to how long it takes to execute -- is recorded on a configuration
 * request and enforced against the observation thereafter.  An observation
 * stays covered while it is open for at least as long as was approved;
 * shortening it, or adding windows where there were none, needs a new request.
 */
class schedulingWindowApproval extends OdbSuite with ObservingModeSetupOperations:

  val admin: User = TestUsers.Standard.admin(3, 32)
  val pi: User    = TestUsers.Standard.pi(1, 30)

  val validUsers: List[User] = List(pi, admin)

  // Inside the default call's active period (2025-02-01 to 2025-07-31).
  private def includeWindow(hours: Int): String =
    s"""[{ inclusion: INCLUDE, startUtc: "2025-03-01T00:00:00Z", end: { after: { hours: $hours } } }]"""

  private def setup: IO[(Program.Id, Target.Id)] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi, "Scheduling window approval")
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

  private def setTooWindow(oid: Observation.Id, window: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { tooWindow: $window } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

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

  private def schedulingWindowHours(oid: Observation.Id): IO[BigDecimal] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            configuration { schedulingWindow { hours } }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "configuration", "schedulingWindow", "hours").require[BigDecimal])

  private def tooWindowJson(oid: Observation.Id): IO[Option[(Boolean, Option[BigDecimal])]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            schedulingConstraints { tooWindow { forever duration { hours } } }
          }
        }
      """
    ).map: js =>
      val c = js.hcursor.downFields("observation", "schedulingConstraints", "tooWindow")
      c.downField("forever").as[Boolean].toOption.map: forever =>
        (forever, c.downFields("duration", "hours").as[BigDecimal].toOption)

  // -- The measure -----------------------------------------------------------

  test("an observation with no timing windows is open for the whole active period"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      hours      <- schedulingWindowHours(oid)
    yield
      // 2025-02-01 to 2025-07-31 inclusive of the start, exclusive of the end.
      assertEquals(hours, BigDecimal(180 * 24))

  test("timing windows narrow it to their total"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, includeWindow(6))
      hours      <- schedulingWindowHours(oid)
    yield assertEquals(hours, BigDecimal(6))

  // -- Enforcement -----------------------------------------------------------

  test("adding a window to an observation approved without one needs a new request"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      rid        <- createConfigurationRequestAs(pi, oid)
      before     <- coveringRequests(oid)
      _          <- setTimingWindows(oid, includeWindow(6))
      after      <- coveringRequests(oid)
    yield
      assertEquals(before, List(rid))
      assertEquals(after, Nil, "A window where there were none is a shortening.")

  test("widening an approved window keeps the observation covered"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, includeWindow(6))
      rid        <- createConfigurationRequestAs(pi, oid)
      _          <- setTimingWindows(oid, includeWindow(12))
      after      <- coveringRequests(oid)
    yield assertEquals(after, List(rid), "Widening needs no approval.")

  test("shortening an approved window needs a new request"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, includeWindow(12))
      _          <- createConfigurationRequestAs(pi, oid)
      _          <- setTimingWindows(oid, includeWindow(6))
      after      <- coveringRequests(oid)
    yield assertEquals(after, Nil)

  test("removing the windows entirely keeps the observation covered"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(oid, includeWindow(6))
      rid        <- createConfigurationRequestAs(pi, oid)
      _          <- setTimingWindows(oid, "[]")
      after      <- coveringRequests(oid)
    yield assertEquals(after, List(rid))

  test("two requests differing only in the scheduling window are distinct"):
    for
      (pid, tid) <- setup
      o1         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(o1, includeWindow(6))
      r1         <- createConfigurationRequestAs(pi, o1)
      o2         <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTimingWindows(o2, includeWindow(12))
      r2         <- createConfigurationRequestAs(pi, o2)
    yield assertNotEquals(r1, r2, s"Expected distinct requests, got $r1 twice.")

  // -- The ToO window --------------------------------------------------------

  test("the ToO window round-trips"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      unset      <- tooWindowJson(oid)
      _          <- setTooWindow(oid, "{ duration: { hours: 6 } }")
      stated     <- tooWindowJson(oid)
      _          <- setTooWindow(oid, "{ forever: true }")
      forever    <- tooWindowJson(oid)
      _          <- setTooWindow(oid, "null")
      cleared    <- tooWindowJson(oid)
    yield
      assertEquals(unset, None)
      assertEquals(stated, (false, BigDecimal(6).some).some)
      assertEquals(forever, (true, none).some)
      assertEquals(cleared, None)

  test("clearing the scheduling constraints clears the ToO window"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTooWindow(oid, "{ duration: { hours: 6 } }")
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
      assertEquals(stated, (false, BigDecimal(6).some).some)
      assertEquals(cleared, None)

  test("an edit that does not mention the scheduling constraints leaves the ToO window alone"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _          <- setTooWindow(oid, "{ duration: { hours: 6 } }")
      _          <- setTimingWindows(oid, includeWindow(3))
      after      <- tooWindowJson(oid)
    yield assertEquals(after, (false, BigDecimal(6).some).some)

  test("a ToO is measured by its ToO window, not by its timing windows"):
    for
      (pid, tid) <- setup
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      ttid       <- createOpportunityTargetAs(pi, pid)
      _          <- addTargetToAsterism(oid, ttid)
      _          <- setSchedulingMode(oid, SchedulingMode.Uninterruptible)
      // Unstated, a ToO makes no scheduling-window demand of its own: how
      // disruptive it may be is the activation ceiling's business.
      unstated   <- schedulingWindowHours(oid)
      _          <- setTooWindow(oid, "{ duration: { hours: 6 } }")
      stated     <- schedulingWindowHours(oid)
      // A timing window is beside the point for a ToO: the trigger supplies one.
      _          <- setTimingWindows(oid, includeWindow(1))
      withWindow <- schedulingWindowHours(oid)
    yield
      assertEquals(unstated, BigDecimal(180 * 24))
      assertEquals(stated, BigDecimal(6))
      assertEquals(withWindow, BigDecimal(6))

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
