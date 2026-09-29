// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
 * Both ToO activations are reachable here because the fixture raises the
 * program's ceiling to the top of the ladder; the ceiling rule itself is covered
 * by tooActivationCeiling, not here.
 */
class tooTriggerActivation extends ExecutionTestSupportForGmos with TooTriggerSetupOperations:

  private def setState(oid: Observation.Id, s: ObservationWorkflowState): IO[Unit] =
    setTooWorkflowState(pi, oid, s)

  private def requestedTrigger(oid: Observation.Id): IO[(TooTrigger.Id, TooActivation, Option[TooTrigger.Id])] =
    getRequestedTooTriggerAs(pi, oid).map: t =>
      (t.id, t.activation, t.supersedes)

  private def allTriggers(oid: Observation.Id): IO[List[(TooTriggerStatus, TooActivation)]] =
    getTooTriggersAs(pi, oid).map: ts =>
      ts.map(t => (t.status, t.activation))

  test("a trigger records the activation it was requested at"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, List(Requested -> Rapid))

  test("an interrupting ToO records a trigger at the top of the ladder"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, activation = TooActivation.Interrupting)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, List(Requested -> Interrupting))

  // Proposed at INTERRUPTING so the ceiling it freezes is evidenced by an
  // observation; lowering is always allowed, and escalating back up stays within
  // that ceiling.  Every test that moves between the two ToO activations starts
  // this way.
  private def createInterrupting: IO[(Program.Id, Observation.Id)] =
    createTooObservationAs(pi, staff, activation = TooActivation.Interrupting).map((pid, oid, _) => (pid, oid))

  test("changing the activation supersedes the request and creates a successor"):
    for
      (_, oid)          <- createInterrupting
      _                 <- setState(oid, ObservationWorkflowState.Ready)
      (first, _, _)     <- requestedTrigger(oid)
      _                 <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      ts                <- allTriggers(oid)
      (_, act, prevOpt) <- requestedTrigger(oid)
    yield
      assertEquals(ts, List(Superseded -> Interrupting, Requested -> Rapid))
      // The successor carries the new activation and points back at the row it replaced.
      assertEquals(act, Rapid)
      assertEquals(prevOpt, Some(first))

  // The case that motivated the whole activation-on-the-trigger design: a live
  // rapid request is escalated to interrupting.
  test("escalating a rapid ToO to interrupting supersedes it and requests a new one"):
    for
      (_, oid)       <- createInterrupting
      _              <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (first, _, _)  <- requestedTrigger(oid)
      _              <- setTooActivationAs(pi, oid, TooActivation.Interrupting)
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
      (_, oid) <- createInterrupting
      _        <- setState(oid, ObservationWorkflowState.Ready)
      _        <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _        <- setTooActivationAs(pi, oid, TooActivation.Interrupting)
      ts       <- allTriggers(oid)
    yield
      // Each closed-out row still says what it was requested at; they are records
      // of what was asked for, not views of what the observation is now.
      assertEquals(
        ts,
        List(Superseded -> Interrupting, Superseded -> Rapid, Requested -> Interrupting)
      )

  test("exactly one request is live through a chain of changes"):
    for
      (_, oid) <- createInterrupting
      _        <- setState(oid, ObservationWorkflowState.Ready)
      _        <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _        <- setTooActivationAs(pi, oid, TooActivation.Interrupting)
      _        <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      ts       <- allTriggers(oid)
    yield
      assertEquals(ts.count(_._1 == Requested), 1)
      assertEquals(ts.count(_._1 == Superseded), 3)

  test("the chain walks back to the first request"):
    for
      (_, oid)       <- createInterrupting
      _              <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (first, _, _)  <- requestedTrigger(oid)
      _              <- setTooActivationAs(pi, oid, TooActivation.Interrupting)
      (second, _, _) <- requestedTrigger(oid)
      _              <- setTooActivationAs(pi, oid, TooActivation.Rapid)
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
      assertEquals(c.downField("tooActivation").require[TooActivation], Rapid)
      assertEquals(c.downFields("supersedes", "id").require[TooTrigger.Id], second)
      assertEquals(c.downFields("supersedes", "tooActivation").require[TooActivation], Interrupting)
      assertEquals(c.downFields("supersedes", "supersedes", "id").require[TooTrigger.Id], first)
      assertEquals(c.downFields("supersedes", "supersedes", "tooActivation").require[TooActivation], Rapid)
      // The root of the chain is the first request, which replaced nothing.
      assertEquals(c.downFields("supersedes", "supersedes", "supersedes").require[Option[Json]], None)

  test("a ToO's mode cannot move, so no mode edit supersedes its request"):
    for
      (_, oid, _)    <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (before, _, _) <- requestedTrigger(oid)
      // Every Target of Opportunity is Uninterruptible, so the mode is not the
      // PI's to change while the activation stands -- and a request's identity
      // could never depend on it.
      _              <- expect(
                          pi,
                          schedulingModeQuery(oid, SchedulingMode.NoSplitting),
                          expected = List(
                            s"Cannot set the scheduling constraints for observation $oid: Target of Opportunity activation RAPID fixes the scheduling mode at UNINTERRUPTIBLE; it cannot be NO_SPLITTING."
                          ).asLeft
                        )
      ts             <- allTriggers(oid)
      (after, _, _)  <- requestedTrigger(oid)
    yield
      assertEquals(ts, List(Requested -> Rapid))
      assertEquals(after, before)

  test("clearing Ready withdraws rather than supersedes"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      _           <- setState(oid, ObservationWorkflowState.Defined)
      ts          <- allTriggers(oid)
    yield assertEquals(ts, List(Withdrawn -> Rapid))

  test("a request made afresh after a withdrawal supersedes nothing"):
    for
      (_, oid)       <- createInterrupting
      _              <- setState(oid, ObservationWorkflowState.Ready)
      _              <- setState(oid, ObservationWorkflowState.Defined)
      _              <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _              <- setState(oid, ObservationWorkflowState.Ready)
      (_, act, prev) <- requestedTrigger(oid)
    yield
      // The activation moved while nothing was outstanding, so this is a first
      // request at RAPID, not a successor to the withdrawn one.
      assertEquals(act, Rapid)
      assertEquals(prev, None)

  test("changing the activation while not triggered records nothing"):
    for
      (_, oid) <- createInterrupting
      _        <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      ts       <- allTriggers(oid)
    yield assertEquals(ts, Nil)

  test("filtering on activation selects the requests that may displace running work"):
    for
      (_, oid) <- createInterrupting
      _        <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      _        <- setTooActivationAs(pi, oid, TooActivation.Interrupting)
      js       <- query(
                       pi,
                       s"""
                         query {
                           tooTriggers(WHERE: {
                             observationId: { EQ: ${oid.asJson} }
                             tooActivation: { GTE: INTERRUPTING }
                           }) {
                             matches { tooActivation }
                           }
                         }
                       """
                     )
    yield
      val acts = js.hcursor.downFields("tooTriggers", "matches").require[List[Json]]
        .map(_.hcursor.downField("tooActivation").require[String]).sorted
      // The superseded RAPID request is excluded.  The ordering is what makes
      // this expressible on the query side, where the filter is real SQL.
      assertEquals(acts, List("INTERRUPTING"))

  test("an executing observation's mode cannot be changed, so its trigger cannot be superseded"):
    for
      (pid, oid, _)  <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
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

  // -- The program's ceiling ---------------------------------------------------
  //
  // Given at acceptance from the observations, and changed afterwards by staff.

  private def setCeiling(pid: Program.Id, ceiling: TooActivation): IO[Unit] =
    query(
      staff,
      s"""
        mutation {
          updatePrograms(input: {
            SET: { tooActivationCeiling: ${ceiling.tag.toScreamingSnakeCase} }
            WHERE: { id: { EQ: "$pid" } }
          }) {
            programs { id }
          }
        }
      """
    ).void

  test("lowering the ceiling withdraws a request it no longer authorizes"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
      _             <- setState(oid, ObservationWorkflowState.Ready)
      before        <- allTriggers(oid)
      // The TAC takes back what it granted.  Nothing about the observation
      // changes, so only the proposal-side trigger can act on this.
      _             <- setCeiling(pid, TooActivation.None)
      after         <- allTriggers(oid)
    yield
      assertEquals(before, List(Requested -> Rapid))
      assertEquals(after, List(Withdrawn -> Rapid))

  test("lowering the ceiling leaves a request it still authorizes alone"):
    for
      (pid, oid)    <- createInterrupting
      _             <- setTooActivationAs(pi, oid, TooActivation.Rapid)
      _             <- setState(oid, ObservationWorkflowState.Ready)
      _             <- setCeiling(pid, TooActivation.Rapid)
      ts            <- allTriggers(oid)
    yield
      // RAPID is at the ceiling, not above it.
      assertEquals(ts, List(Requested -> Rapid))

  test("raising the ceiling withdraws nothing"):
    for
      (pid, oid, _) <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
      _             <- setState(oid, ObservationWorkflowState.Ready)
      _             <- setCeiling(pid, TooActivation.Interrupting)
      ts            <- allTriggers(oid)
    yield assertEquals(ts, List(Requested -> Rapid))

  // A Defined observation may be raised freely and simply goes Unapproved; a Ready
  // one may not, since the raise would supersede its request with one at an
  // activation nobody approved.
  test("a Ready ToO cannot be raised above the ceiling"):
    for
      (_, oid, _) <- createTooObservationAs(pi, staff, activation = TooActivation.Rapid)
      _           <- setState(oid, ObservationWorkflowState.Ready)
      _           <- expect(
                       pi,
                       tooActivationQuery(oid, TooActivation.Interrupting),
                       List(
                         s"Cannot set the Target of Opportunity activation for observation $oid while it is Ready: INTERRUPTING is above RAPID, the program's ceiling. Set it back to Defined, or have staff raise the ceiling first."
                       ).asLeft
                     )
      ts          <- allTriggers(oid)
    yield
      // Refused outright, supersession included.
      assertEquals(ts, List(Requested -> Rapid))
