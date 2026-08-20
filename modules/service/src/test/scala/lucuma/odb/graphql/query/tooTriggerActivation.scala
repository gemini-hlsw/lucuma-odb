// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.TooActivation
import lucuma.core.enums.TooActivation.Interrupting
import lucuma.core.enums.TooActivation.Rapid
import lucuma.core.enums.TooActivation.Standard
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.string.*
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus
import lucuma.odb.data.TooTriggerStatus.*

/**
 * The activation a trigger was requested at, and what happens when the
 * observation's activation moves while the request is outstanding.
 *
 * The activation is written when the trigger is created and never changes: a
 * request at a different activation is a different request, since who is
 * notified and how fast both differ.  So a change supersedes the outstanding row
 * and creates a successor linked back to it, rather than amending it in place.
 *
 * All three ToO activations are reachable here because the fixture raises the
 * proposal's ceiling to the top of the ladder; the ceiling rule itself is covered
 * by tooActivationCeiling, not here.
 */
class tooTriggerActivation extends ExecutionTestSupportForGmos with TooTriggerSetupOperations:

  private def setState(oid: Observation.Id, s: ObservationWorkflowState): IO[Unit] =
    setTooWorkflowState(pi, oid, s)

  private def setMode(oid: Observation.Id, mode: SchedulingMode): IO[Unit] =
    setSchedulingModeAs(pi, oid, mode)

  private def requestedTrigger(oid: Observation.Id): IO[(TooTrigger.Id, TooActivation, Option[TooTrigger.Id])] =
    getRequestedTooTriggerAs(pi, oid).map: t =>
      (t.id, t.activation, t.supersedes)

  private def allTriggers(oid: Observation.Id): IO[List[(TooTriggerStatus, TooActivation)]] =
    getTooTriggersAs(pi, oid).map: ts =>
      ts.map(t => (t.status, t.activation))

  private def schedulingModeQuery(oid: Observation.Id, mode: SchedulingMode): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { schedulingConstraints: { schedulingMode: ${mode.tag.toScreamingSnakeCase} } }
          WHERE: { id: { EQ: ${oid.asJson} } }
        }) {
          observations { id }
        }
      }
    """

  test("a trigger records the activation it was requested at"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, List(Requested -> Rapid))

  test("an interrupting ToO records a trigger at the top of the ladder"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Interrupting)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, List(Requested -> Interrupting))

  test("changing the activation supersedes the request and creates a successor"):
    for
      (_, oid, _)       <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _                 <- setState(oid, ObservationWorkflowState.Ready)
      (first, _, _)     <- requestedTrigger(oid)
      _                 <- setMode(oid, SchedulingMode.Unconstrained)
      ts                <- allTriggers(oid)
      (_, act, prevOpt) <- requestedTrigger(oid)
    yield
      assertEquals(ts, List(Superseded -> Rapid, Requested -> Standard))
      // The successor carries the new activation and points back at the row it replaced.
      assertEquals(act, Standard)
      assertEquals(prevOpt, Some(first))

  // The case that motivated the whole activation-on-the-trigger design: a live
  // rapid request is escalated to interrupting.
  test("escalating a rapid ToO to interrupting supersedes it and requests a new one"):
    for
      (_, oid, _)    <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (first, _, _)  <- requestedTrigger(oid)
      _              <- setMode(oid, SchedulingMode.Interrupting)
      ts             <- allTriggers(oid)
      (_, act, prev) <- requestedTrigger(oid)
    yield
      assertEquals(ts, List(Superseded -> Rapid, Requested -> Interrupting))
      // The new request carries the escalated activation and points back at the
      // one it replaced; the closed-out row still says RAPID, which is what was
      // actually asked for at the time.
      assertEquals(act, Interrupting)
      assertEquals(prev, Some(first))

  test("a superseded request keeps the activation it was made at"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      _           <- setMode(oid, SchedulingMode.Unconstrained)
      _           <- setMode(oid, SchedulingMode.Uninterruptible)
      ts          <- allTriggers(oid)
    yield
      // Each closed-out row still says what it was requested at; they are records
      // of what was asked for, not views of what the observation is now.
      assertEquals(
        ts,
        List(Superseded -> Rapid, Superseded -> Standard, Requested -> Rapid)
      )

  test("exactly one request is live through a chain of changes"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      _           <- setMode(oid, SchedulingMode.Unconstrained)
      _           <- setMode(oid, SchedulingMode.Uninterruptible)
      _           <- setMode(oid, SchedulingMode.Unconstrained)
      ts          <- allTriggers(oid)
    yield
      assertEquals(ts.count(_._1 == Requested), 1)
      assertEquals(ts.count(_._1 == Superseded), 3)

  test("the chain walks back to the first request"):
    for
      (_, oid, _)    <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (first, _, _)  <- requestedTrigger(oid)
      _              <- setMode(oid, SchedulingMode.Unconstrained)
      (second, _, _) <- requestedTrigger(oid)
      _              <- setMode(oid, SchedulingMode.Interrupting)
      js             <- query(
                          pi,
                          s"""
                            query {
                              tooTriggers(WHERE: { observationId: { EQ: ${oid.asJson} }, status: { EQ: REQUESTED } }) {
                                matches {
                                  id
                                  tooActivation
                                  supersedes {
                                    id
                                    tooActivation
                                    supersedes { id tooActivation supersedes { id } }
                                  }
                                }
                              }
                            }
                          """
                        )
    yield
      val c = js.hcursor.downFields("tooTriggers", "matches").require[List[Json]].head.hcursor
      assertEquals(c.downField("tooActivation").require[TooActivation], Interrupting)
      assertEquals(c.downFields("supersedes", "id").require[TooTrigger.Id], second)
      assertEquals(c.downFields("supersedes", "tooActivation").require[TooActivation], Standard)
      assertEquals(c.downFields("supersedes", "supersedes", "id").require[TooTrigger.Id], first)
      assertEquals(c.downFields("supersedes", "supersedes", "tooActivation").require[TooActivation], Rapid)
      // The root of the chain is the first request, which replaced nothing.
      assertEquals(c.downFields("supersedes", "supersedes", "supersedes").require[Option[Json]], None)

  test("a mode change that does not move the activation supersedes nothing"):
    for
      (_, oid, _)    <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      _              <- setMode(oid, SchedulingMode.Unconstrained)
      (before, _, _) <- requestedTrigger(oid)
      // Both UNCONSTRAINED and NO_SPLITTING derive STANDARD, so nothing changes.
      _              <- setMode(oid, SchedulingMode.NoSplitting)
      ts             <- allTriggers(oid)
      (after, _, _)  <- requestedTrigger(oid)
    yield
      assertEquals(ts.count(_._1 == Superseded), 1)
      assertEquals(after, before)

  test("clearing Ready withdraws rather than supersedes"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      _           <- setState(oid, ObservationWorkflowState.Defined)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, List(Withdrawn -> Rapid))

  test("a request made afresh after a withdrawal supersedes nothing"):
    for
      (_, oid, _)    <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      _              <- setState(oid, ObservationWorkflowState.Defined)
      _              <- setMode(oid, SchedulingMode.Unconstrained)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (_, act, prev) <- requestedTrigger(oid)
    yield
      // The activation moved while nothing was outstanding, so this is a first
      // request at STANDARD, not a successor to the withdrawn one.
      assertEquals(act, Standard)
      assertEquals(prev, None)

  test("changing the activation while not triggered records nothing"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setMode(oid, SchedulingMode.Unconstrained)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, Nil)

  test("filtering on activation selects the requests that cannot wait for the queue"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      _           <- setMode(oid, SchedulingMode.Unconstrained)
      _           <- setMode(oid, SchedulingMode.Interrupting)
      js          <- query(
                       pi,
                       s"""
                         query {
                           tooTriggers(WHERE: {
                             observationId: { EQ: ${oid.asJson} }
                             tooActivation: { GTE: RAPID }
                           }) {
                             matches { tooActivation }
                           }
                         }
                       """
                     )
    yield
      val acts = js.hcursor.downFields("tooTriggers", "matches").require[List[Json]]
        .map(_.hcursor.downField("tooActivation").require[String]).sorted
      // The STANDARD request is excluded.  The ordering is what makes this
      // expressible on the query side, where the filter is real SQL.  RAPID does
      // not displace work already under way -- only INTERRUPTING does -- but both
      // are wanted sooner than the queue would get to them.
      assertEquals(acts, List("INTERRUPTING", "RAPID"))

  test("an executing observation's mode cannot be changed, so its trigger cannot be superseded"):
    for
      (pid, oid, _)  <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (before, _, _) <- requestedTrigger(oid)
      // One completed step is enough to be under way without finishing.
      v              <- recordVisitAs(serviceUser, oid)
      s              <- firstScienceAtomStepIds(serviceUser, oid)
      _              <- addEndStepEvent(s.head, v)
      state          <- tooWorkflowState(pid, oid, pi)
      // Scheduling edits are limited to the pre-execution states, and the refusal
      // is explicit rather than a silently empty update.
      _              <- expect(
                          pi,
                          schedulingModeQuery(oid, SchedulingMode.Unconstrained),
                          expected = List(
                            s"Observation $oid is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Completed)."
                          ).asLeft
                        )
      ts             <- allTriggers(oid)
      ids            <- getTooTriggersAs(pi, oid).map(_.map(_.id))
    yield
      assertEquals(state, ObservationWorkflowState.Ongoing)
      // The request survives at the activation it was made at, and by now it has
      // been accepted -- execution is what accepts it.  A running observation
      // cannot have its trigger replaced out from under it, and there is no longer
      // a live request to replace.
      assertEquals(ts, List(Accepted -> Rapid))
      // Still the very same row, accepted rather than superseded by a successor.
      assertEquals(ids, List(before))

  /** Lowers (or raises) the proposal's explicit ceiling, which only staff may do. */
  private def setCeiling(pid: Program.Id, ceiling: String): IO[Unit] =
    query(
      staff,
      s"""
        mutation {
          updateProposal(input: {
            programId: "$pid"
            SET: { gemini: { queue: { explicitTooActivationCeiling: $ceiling } } }
          }) {
            proposal { gemini { ... on Queue { explicitTooActivationCeiling } } }
          }
        }
      """
    ).void

  test("lowering the ceiling withdraws a request it no longer authorizes"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _             <- setState(oid, ObservationWorkflowState.Ready)
      before        <- allTriggers(oid)
      // The TAC takes back what it granted.  Nothing about the observation
      // changes, so only the proposal-side trigger can act on this.
      _             <- setCeiling(pid, "STANDARD")
      after         <- allTriggers(oid)
    yield
      assertEquals(before, List(Requested -> Rapid))
      assertEquals(after, List(Withdrawn -> Rapid))

  test("lowering the ceiling leaves a request it still authorizes alone"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Unconstrained)
      _             <- setState(oid, ObservationWorkflowState.Ready)
      _             <- setCeiling(pid, "STANDARD")
      ts            <- allTriggers(oid)
    yield
      // STANDARD is at the ceiling, not above it.
      assertEquals(ts, List(Requested -> Standard))

  test("raising the ceiling withdraws nothing"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, mode = SchedulingMode.Uninterruptible)
      _             <- setState(oid, ObservationWorkflowState.Ready)
      _             <- setCeiling(pid, "INTERRUPTING")
      ts            <- allTriggers(oid)
    yield assertEquals(ts, List(Requested -> Rapid))