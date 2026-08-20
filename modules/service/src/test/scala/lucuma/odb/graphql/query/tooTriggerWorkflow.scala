// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SchedulingMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus
import lucuma.odb.data.TooTriggerStatus.*

/**
 * The Target-of-Opportunity trigger, derived from the observation's workflow
 * state: setting a ToO observation `Ready` requests a trigger, clearing that
 * state withdraws it, and the database keeps the two in step.  Declining is the
 * one action an observer takes, and it returns the observation to `Defined`.
 */
class tooTriggerWorkflow extends ExecutionTestSupportForGmos with TooTriggerSetupOperations:

  private def triggers(oid: Observation.Id): IO[List[(TooTriggerStatus, Option[String])]] =
    getTooTriggersAs(pi, oid).map: ts =>
      ts.map(t => (t.status, t.resolution))

  private def triggerId(oid: Observation.Id): IO[TooTrigger.Id] =
    getRequestedTooTriggerAs(pi, oid).map(_._1)

  private def getWorkflowState(pid: Program.Id, oid: Observation.Id): IO[ObservationWorkflowState] =
    tooWorkflowState(pid, oid, pi)

  private def setWorkflowState(oid: Observation.Id, s: ObservationWorkflowState): IO[Unit] =
    setTooWorkflowState(pi, oid, s)

  test("setting a ToO observation Ready requests a trigger"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff)
      before     <- triggers(oid)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      after      <- triggers(oid)
      s          <- getWorkflowState(pid, oid)
    yield
      assertEquals(before, Nil)
      assertEquals(after, List((Requested, None)))
      assertEquals(s, ObservationWorkflowState.Ready)

  test("clearing Ready withdraws the trigger"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Defined)
      ts         <- triggers(oid)
      s          <- getWorkflowState(pid, oid)
    yield
      assertEquals(ts, List((Withdrawn, None)))
      assertEquals(s, ObservationWorkflowState.Defined)

  test("marking a triggered observation Inactive withdraws the trigger"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Inactive)
      ts         <- triggers(oid)
      s          <- getWorkflowState(pid, oid)
    yield
      assertEquals(ts, List((Withdrawn, None)))
      assertEquals(s, ObservationWorkflowState.Inactive)

  test("re-triggering after a withdrawal creates a second trigger, keeping the first as history"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Defined)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      ts       <- triggers(oid)
    yield assertEquals(ts.map(_._1), List(Withdrawn, Requested))

  test("a non-ToO observation set Ready records no trigger"):
    for
      (pid, oid) <- createTriggerableObservationAs(pi, staff)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      ts         <- triggers(oid)
      s          <- getWorkflowState(pid, oid)
    yield
      assertEquals(ts, Nil)
      assertEquals(s, ObservationWorkflowState.Ready)

  // The activation is derived from the asterism now, so what used to be
  // "lower the activation" is "remove the opportunity target" -- the
  // observation stops being a ToO and its request goes with it.
  test("removing the opportunity target while Ready withdraws the trigger"):
    for
      (_, oid, tid) <- createTooObservationAs(pi, staff)
      _             <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _             <- editAsterismAs(pi, oid, add = Nil, del = List(tid))
      ts            <- triggers(oid)
    yield assertEquals(ts, List((Withdrawn, None)))

  test("adding a resolved opportunity target while Ready requests a trigger"):
    for
      (pid, oid) <- createTriggerableObservationAs(pi, staff)
      tid        <- createOpportunityTargetAs(pi, pid)
      _          <- resolveOpportunityTargetAs(pi, tid)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _          <- editAsterismAs(pi, oid, add = List(tid), del = Nil)
      ts         <- triggers(oid)
    yield assertEquals(ts, List((Requested, None)))

  test("declining records the reason and returns the observation to Defined"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      rid        <- triggerId(oid)
      _          <- expect(
                      staff,
                      declineQuery(rid, "weathered out".some),
                      expected = json"""
                        {
                          "declineTooTrigger" : {
                            "tooTrigger" : {
                              "status" : "DECLINED",
                              "resolutionReason" : "weathered out"
                            }
                          }
                        }
                      """.asRight
                    )
      ts         <- triggers(oid)
      s          <- getWorkflowState(pid, oid)
    yield
      // Declined, not withdrawn: the reason survives the user-state clear.
      assertEquals(ts, List((Declined, Some("weathered out"))))
      assertEquals(s, ObservationWorkflowState.Defined)

  test("a declined trigger does not block a fresh request"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      rid      <- triggerId(oid)
      _        <- query(staff, declineQuery(rid))
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      ts       <- triggers(oid)
    yield assertEquals(ts.map(_._1), List(Declined, Requested))

  test("a PI cannot decline"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      rid      <- triggerId(oid)
      _        <- expect(
                    pi,
                    declineQuery(rid),
                    expected = List(s"User ${pi.id} is not authorized to perform this operation.").asLeft
                  )
    yield ()

  test("an already-declined trigger cannot be declined again"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff)
      _        <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      rid      <- triggerId(oid)
      _        <- query(staff, declineQuery(rid))
      _        <- expect(
                    staff,
                    declineQuery(rid),
                    expected = List(s"TooTrigger $rid could not be declined (not found, or no longer requested).").asLeft
                  )
    yield ()

  // The old incoherence -- an opportunity target with NONE activation -- is now
  // unrepresentable, since the activation is derived from the target.  What is
  // still possible is its converse: INTERRUPTING is the one mode reserved to
  // Targets of Opportunity, so carrying it without one is rejected.
  test("an interrupting observation with no opportunity target is Undefined"):
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setSchedulingModeAs(pi, oid, SchedulingMode.Interrupting)
      s   <- getWorkflowState(pid, oid)
      ms  <- query(
               pi,
               s"""
                 query {
                   observation(observationId: ${oid.asJson}) {
                     workflow { value { validationErrors { messages } } }
                   }
                 }
               """
             ).map(_.hcursor.downFields("observation", "workflow", "value", "validationErrors")
                     .require[List[io.circe.Json]]
                     .flatMap(_.hcursor.downField("messages").require[List[String]]))
    yield
      assertEquals(s, ObservationWorkflowState.Undefined)
      assert(ms.exists(_.contains("may only interrupt executing science")), s"expected the interrupting message, got $ms")

  test("an observation still holding an unresolved opportunity target cannot be triggered"):
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createOpportunityTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- getWorkflowState(pid, oid)
      // Defined -> Ready excludes an unresolved opportunity target, so there is
      // no way to request a trigger while it is still waiting on the alert.
      r   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready).attempt
      ts  <- triggers(oid)
    yield
      assert(r.isLeft, s"expected the transition to be refused, got $r")
      assertEquals(ts, Nil)

  // The gate above must key on *unresolved*-ness, not on the presence of an
  // opportunity target.  The target keeps its identity when the alert arrives
  // rather than being replaced, so gating on presence alone would make a
  // triggerable ToO impossible to trigger -- and every other trigger test here
  // uses an ordinary target with a declared activation, so nothing else covers
  // this path.
  test("a resolved opportunity target is offered Ready"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, resolved = true)
      (s, ts)       <- tooWorkflowStateAndTransitions(pid, oid, pi)
    yield
      assertEquals(s, ObservationWorkflowState.Defined)
      assert(ts.contains(ObservationWorkflowState.Ready), s"expected READY among the transitions, got $ts")

  test("an unresolved opportunity target is not offered Ready"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, resolved = false)
      (_, ts)       <- tooWorkflowStateAndTransitions(pid, oid, pi)
    yield assert(!ts.contains(ObservationWorkflowState.Ready), s"expected READY to be withheld, got $ts")

  test("setting a resolved opportunity ToO Ready requests a trigger"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, resolved = true)
      before      <- triggers(oid)
      _           <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      after       <- triggers(oid)
    yield
      assertEquals(before, Nil)
      assertEquals(after, List((Requested, None)))

  test("resolving an opportunity target unblocks triggering"):
    for
      (pid, oid, tid) <- createTooObservationAs(pi, staff, resolved = false)
      (_, before)     <- tooWorkflowStateAndTransitions(pid, oid, pi)
      // No re-approval: the request was approved against the target's region, and
      // resolving inside that region leaves the approval covering it.
      _               <- resolveOpportunityTargetAs(pi, tid)
      (_, after)      <- tooWorkflowStateAndTransitions(pid, oid, pi)
      _               <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      ts              <- triggers(oid)
    yield
      assert(!before.contains(ObservationWorkflowState.Ready), s"expected READY to be withheld, got $before")
      assert(after.contains(ObservationWorkflowState.Ready), s"expected READY once resolved, got $after")
      assertEquals(ts, List((Requested, None)))

  // The approval is against the region, so the resolution has to land inside it.  This is the
  // one place a ToO's region is enforced today, and it enforces it through the approval rather
  // than through a validator of its own: `Configuration.subsumes` asks `region.contains(coords)`.
  test("resolving inside the approved region keeps the approval"):
    for
      (pid, oid, tid) <- createTooObservationAs(pi, staff, resolved = false)
      _               <- resolveOpportunityTargetAs(pi, tid, "30:00:00.00")
      state           <- tooWorkflowState(pid, oid, pi)
    yield assertEquals(state, ObservationWorkflowState.Defined)

  test("resolving outside the approved region unapproves the observation"):
    for
      (pid, oid, tid) <- createTooObservationAs(pi, staff, resolved = false)
      // The region runs from 10 to 70 degrees of declination; this is below it.
      _               <- resolveOpportunityTargetAs(pi, tid, "-00:06:04.89")
      (state, trans)  <- tooWorkflowStateAndTransitions(pid, oid, pi)
    yield
      assertEquals(state, ObservationWorkflowState.Unapproved)
      assert(!trans.contains(ObservationWorkflowState.Ready), s"expected READY to be withheld, got $trans")

  // The region outlives resolution, so the approval keeps being checked against it: moving a
  // resolved target out of its region later is caught exactly as resolving outside it would be.
  test("moving a resolved target outside the region unapproves it afterwards"):
    for
      (pid, oid, tid) <- createTooObservationAs(pi, staff, resolved = false)
      _               <- resolveOpportunityTargetAs(pi, tid, "30:00:00.00")
      inside          <- tooWorkflowState(pid, oid, pi)
      _               <- resolveOpportunityTargetAs(pi, tid, "-00:06:04.89")
      outside         <- tooWorkflowState(pid, oid, pi)
      _               <- resolveOpportunityTargetAs(pi, tid, "45:00:00.00")
      back            <- tooWorkflowState(pid, oid, pi)
    yield
      assertEquals(inside,  ObservationWorkflowState.Defined)
      assertEquals(outside, ObservationWorkflowState.Unapproved)
      assertEquals(back,    ObservationWorkflowState.Defined)

  private def unresolveTargetAs(tid: lucuma.core.model.Target.Id): IO[Unit] =
    query(pi,
      s"""
        mutation {
          updateTargets(input: {
            SET: { opportunity: { resolution: null } }
            WHERE: { id: { EQ: ${tid.asJson} } }
          }) { targets { id } }
        }
      """
    ).void

  // An unresolved ToO has nowhere to point, so it must not put a request in front of an observer.
  // The workflow validator calls such an observation Undefined, but that is a computed opinion --
  // it does not reach into t_too_trigger -- so resolvedness has to be part of the trigger's own
  // predicate.  Both of these created or kept a live trigger before it was.
  test("adding an unresolved opportunity target while Ready records no trigger"):
    for
      (pid, oid) <- createTriggerableObservationAs(pi, staff)
      tid        <- createOpportunityTargetAs(pi, pid)
      _          <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _          <- editAsterismAs(pi, oid, add = List(tid), del = Nil)
      ts         <- triggers(oid)
    yield assertEquals(ts, Nil)

  test("clearing the resolution of a triggered ToO withdraws the trigger"):
    for
      (_, oid, tid) <- createTooObservationAs(pi, staff, resolved = true)
      _             <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      before        <- triggers(oid)
      _             <- unresolveTargetAs(tid)
      after         <- triggers(oid)
    yield
      assertEquals(before, List((Requested, None)))
      assertEquals(after,  List((Withdrawn, None)))

  // ... and resolving it again asks afresh, so the round trip is not one-way.
  test("re-resolving a withdrawn trigger requests it again"):
    for
      (_, oid, tid) <- createTooObservationAs(pi, staff, resolved = true)
      _             <- setWorkflowState(oid, ObservationWorkflowState.Ready)
      _             <- unresolveTargetAs(tid)
      _             <- resolveOpportunityTargetAs(pi, tid)
      ts            <- triggers(oid)
    yield assertEquals(ts.map(_._1), List(Withdrawn, Requested))
