// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.math.Coordinates
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.TestCoordinates.coords

/**
 * The approved region outlives the opportunity target that introduced it.
 *
 * A configuration request is a standalone row recording the configuration an
 * observation had when the request was made -- for an unresolved Target of
 * Opportunity, that configuration carries the target's *region* rather than any
 * coordinates.  Nothing recomputes the row afterwards; approval is matched to an
 * observation on demand by asking whether the request `subsumes` the
 * observation's current configuration.
 *
 * So the region keeps being enforced after the opportunity target is gone:
 *
 *   - swap it for an ordinary sidereal target and `subsumes` asks
 *     `region.contains(coords)`;
 *   - stand up a *new* ToO observation beside it and `subsumes` asks
 *     `region.containsAll(otherRegion)`, so a region no larger than the approved
 *     one is covered and one reaching outside it is not.
 *
 * The second case is what lets a program mint further ToO observations against an
 * approval it already holds, and it needs no new configuration request.
 */
class tooRegionApproval extends ExecutionTestSupportForGmos with TooTriggerSetupOperations:

  // Inside the approved declination arc (10 to 70 degrees).
  private val Inside = coords("05:46:13.137 +30:00:00.00")

  /**
   * A ToO program whose one approved configuration request records the region of
   * an unresolved opportunity target: all of RA, declination 10 to 70 degrees.
   */
  private def approvedRegion: IO[(Program.Id, Observation.Id, Target.Id)] =
    createTooObservationAs(pi, staff, resolved = false)

  /** Replaces the observation's opportunity target with an ordinary sidereal one. */
  private def swapInSiderealAt(
    pid:    Program.Id,
    oid:    Observation.Id,
    tooTid: Target.Id,
    c:      Coordinates
  ): IO[Target.Id] =
    for
      tid <- createSiderealTargetAtAs(pi, pid, c)
      _   <- editAsterismAs(pi, oid, add = List(tid), del = List(tooTid))
      _   <- computeItcResultAs(pi, oid)
    yield tid

  /**
   * Adds another ToO observation to the program, with an opportunity target
   * drawing `region`.  Deliberately creates no configuration request of its own:
   * whether it is approved is entirely a question of whether the program's
   * existing approval covers it.
   */
  private def addTooObservation(pid: Program.Id, region: String): IO[(Observation.Id, Target.Id)] =
    for
      tid <- createOpportunityTargetWithRegionAs(pi, pid, region)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- computeItcResultAs(pi, oid)
    yield (oid, tid)

  /** The approved requests that cover this observation, if any. */
  private def requestsFor(oid: Observation.Id): IO[List[ConfigurationRequest.Id]] =
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
        .require[List[Json]]
        .map(_.hcursor.downField("id").require[ConfigurationRequest.Id])

  private def stateOf(pid: Program.Id, oid: Observation.Id): IO[ObservationWorkflowState] =
    tooWorkflowState(pid, oid, pi)

  test("swapping the opportunity target for a sidereal one inside the region keeps the approval"):
    for
      (pid, oid, tid) <- approvedRegion
      before          <- requestsFor(oid)
      _               <- swapInSiderealAt(pid, oid, tid, Inside)
      after           <- requestsFor(oid)
      (state, trans)  <- tooWorkflowStateAndTransitions(pid, oid, pi)
    yield
      assertEquals(before.size, 1, "the unresolved ToO should be covered by its region request")
      assertEquals(after, before, "the same request should still cover the swapped observation")
      assertEquals(state, ObservationWorkflowState.Defined)
      assert(trans.contains(ObservationWorkflowState.Ready), s"expected READY to be offered, got $trans")

  test("a new ToO observation whose region equals the approved one is covered by it"):
    for
      (pid, oid, tid) <- approvedRegion
      _               <- swapInSiderealAt(pid, oid, tid, Inside)
      approved        <- requestsFor(oid)
      (oid2, _)       <- addTooObservation(pid, DefaultOpportunityRegion)
      covering        <- requestsFor(oid2)
      state           <- stateOf(pid, oid2)
    yield
      assertEquals(covering, approved, "an identical region should match the same approved request")
      assertEquals(state, ObservationWorkflowState.Defined)

  test("a new ToO observation whose region is smaller than the approved one is covered by it"):
    for
      (pid, oid, tid) <- approvedRegion
      _               <- swapInSiderealAt(pid, oid, tid, Inside)
      approved        <- requestsFor(oid)
      // Strictly inside the approved declination arc of 10 to 70 degrees.
      (oid2, _)       <- addTooObservation(pid, decRegion(20, 60))
      covering        <- requestsFor(oid2)
      state           <- stateOf(pid, oid2)
      first           <- stateOf(pid, oid)
    yield
      assertEquals(covering, approved, "a smaller region should match the same approved request")
      assertEquals(state, ObservationWorkflowState.Defined)
      assertEquals(first, ObservationWorkflowState.Defined, "the swapped observation should be unaffected")

  test("a new ToO observation whose region falls outside the approved one is Unapproved"):
    for
      (pid, oid, tid) <- approvedRegion
      _               <- swapInSiderealAt(pid, oid, tid, Inside)
      // Entirely below the approved declination arc of 10 to 70 degrees.
      (oid2, _)       <- addTooObservation(pid, decRegion(-70, -10))
      covering        <- requestsFor(oid2)
      (state, trans)  <- tooWorkflowStateAndTransitions(pid, oid2, pi)
      first           <- stateOf(pid, oid)
    yield
      assertEquals(covering, Nil, "a region reaching outside the approved one should match nothing")
      assertEquals(state, ObservationWorkflowState.Unapproved)
      assert(!trans.contains(ObservationWorkflowState.Ready), s"expected READY to be withheld, got $trans")
      assertEquals(first, ObservationWorkflowState.Defined, "the swapped observation should be unaffected")

  test("a new ToO observation whose region only partly overlaps the approved one is Unapproved"):
    for
      (pid, oid, tid) <- approvedRegion
      _               <- swapInSiderealAt(pid, oid, tid, Inside)
      // Straddles the lower edge of the approved arc: overlapping is not containment.
      (oid2, _)       <- addTooObservation(pid, decRegion(-10, 30))
      covering        <- requestsFor(oid2)
      state           <- stateOf(pid, oid2)
    yield
      assertEquals(covering, Nil, "partial overlap should not be treated as coverage")
      assertEquals(state, ObservationWorkflowState.Unapproved)

  /**
   * Two ToO observations in one program, alike in everything but the region their
   * opportunity targets draw, each with its own approved configuration request.
   */
  private def twoApprovedRegions: IO[(Program.Id, Observation.Id, Target.Id, ConfigurationRequest.Id, ConfigurationRequest.Id)] =
    for
      cfp  <- createGeminiCallForProposalsAs(staff)
      pid  <- createProgramWithNonPartnerPi(pi, "ToO")
      _    <- addProposal(pi, pid, cfp.some, None)
      tidA <- createOpportunityTargetWithRegionAs(pi, pid, decRegion(10, 70))
      oidA <- createGmosNorthLongSlitObservationAs(pi, pid, List(tidA))
      rA   <- createConfigurationRequestAs(pi, oidA)
      _    <- setConfigurationRequestStatusAs(staff, rA, ConfigurationRequestStatus.Approved)
      _    <- computeItcResultAs(pi, oidA)
      tidB <- createOpportunityTargetWithRegionAs(pi, pid, decRegion(-70, -10))
      oidB <- createGmosNorthLongSlitObservationAs(pi, pid, List(tidB))
      rB   <- createConfigurationRequestAs(pi, oidB)
      _    <- setConfigurationRequestStatusAs(staff, rB, ConfigurationRequestStatus.Approved)
      _    <- computeItcResultAs(pi, oidB)
      _    <- addPartnerSplits(pi, pid)
      _    <- addCoisAs(pi, pid)
      _    <- setProposalStatus(staff, pid, "ACCEPTED")
    yield (pid, oidA, tidA, rA, rB)

  // These pin the program-wide half of approval: a configuration request records no
  // observation, so `selectRequests` asks every request in the program whether it
  // subsumes the observation's current configuration. Two region-bearing requests in one
  // program agreeing on conditions and observing mode only became possible once
  // V1298__config_request_unique_key.sql rebuilt `t_configuration_request_unique` to
  // carry the six `c_region_*` columns; before that the second insert collided and
  // `canonicalizeRequest` raised "likely due to an incorrect unique index".

  // Approval is program-scoped.  A configuration request records no observation, and
  // `selectRequests` asks every request in the program whether it subsumes the
  // observation's current configuration -- so for observations alike in conditions and
  // observing mode, the approved regions effectively union.  Observation A may be
  // pointed anywhere B was approved for.
  test("a swapped target inside *another* observation's approved region is still approved"):
    for
      (pid, oidA, tidA, rA, rB) <- twoApprovedRegions
      before                    <- requestsFor(oidA)
      // Inside B's arc (-70 to -10) and well outside A's own (10 to 70).
      _                         <- swapInSiderealAt(pid, oidA, tidA, coords("05:46:13.137 -30:00:00.00"))
      after                     <- requestsFor(oidA)
      (state, trans)            <- tooWorkflowStateAndTransitions(pid, oidA, pi)
    yield
      assertEquals(before, List(rA), "A should start out covered by its own region request")
      assertEquals(after, List(rB), "A should end up covered by B's region request instead")
      assertEquals(state, ObservationWorkflowState.Defined)
      assert(trans.contains(ObservationWorkflowState.Ready), s"expected READY to be offered, got $trans")

  // The corollary: land outside *both* regions and nothing covers it.
  test("a swapped target outside every approved region is Unapproved"):
    for
      (pid, oidA, tidA, _, _) <- twoApprovedRegions
      _                       <- swapInSiderealAt(pid, oidA, tidA, coords("05:46:13.137 +80:00:00.00"))
      after                   <- requestsFor(oidA)
      state                   <- stateOf(pid, oidA)
    yield
      assertEquals(after, Nil)
      assertEquals(state, ObservationWorkflowState.Unapproved)
