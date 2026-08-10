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
import lucuma.core.model.Observation
import lucuma.core.model.Program

/**
 * The Target-of-Opportunity trigger, derived from the observation's workflow
 * state: setting a ToO observation `Ready` requests a trigger, clearing that
 * state withdraws it, and the database keeps the two in step.  Declining is the
 * one action an observer takes, and it returns the observation to `Defined`.
 */
class tooTriggerWorkflow extends ExecutionTestSupportForGmos with TooTriggerSetupOperations:

  private def triggers(oid: Observation.Id): IO[List[(String, Option[String])]] =
    query(
      pi,
      s"""
        query {
          tooTriggers(WHERE: { observationId: { EQ: ${oid.asJson} } }) {
            matches { status resolutionReason }
          }
        }
      """
    ).map:
      _.hcursor.downFields("tooTriggers", "matches").require[List[io.circe.Json]].map: j =>
        (
          j.hcursor.downField("status").require[String],
          j.hcursor.downField("resolutionReason").require[Option[String]]
        )

  private def triggerId(oid: Observation.Id): IO[String] =
    query(
      pi,
      s"""
        query {
          tooTriggers(WHERE: { observationId: { EQ: ${oid.asJson} }, status: { EQ: REQUESTED } }) {
            matches { id }
          }
        }
      """
    ).map(_.hcursor.downFields("tooTriggers", "matches").require[List[io.circe.Json]].head.hcursor.downField("id").require[String])

  private def declineQuery(rid: String, reason: Option[String] = None): String =
    s"""
      mutation {
        declineTooTrigger(input: {
          tooTriggerId: "$rid"
          ${reason.fold("")(r => s"""reason: "$r"""")}
        }) {
          tooTrigger { status resolutionReason }
        }
      }
    """

  private def state(pid: Program.Id, oid: Observation.Id): IO[String] =
    tooWorkflowState(pid, oid, pi)

  private def setState(oid: Observation.Id, s: ObservationWorkflowState): IO[Unit] =
    setTooWorkflowState(pi, oid, s)

  test("setting a ToO observation Ready requests a trigger"):
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      before     <- triggers(oid)
      _          <- setState(oid, ObservationWorkflowState.Ready)
      after      <- triggers(oid)
      s          <- state(pid, oid)
    yield
      assertEquals(before, Nil)
      assertEquals(after, List(("REQUESTED", None)))
      assertEquals(s, "READY")

  test("clearing Ready withdraws the trigger"):
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      _          <- setState(oid, ObservationWorkflowState.Ready)
      _          <- setState(oid, ObservationWorkflowState.Defined)
      ts         <- triggers(oid)
      s          <- state(pid, oid)
    yield
      assertEquals(ts, List(("WITHDRAWN", None)))
      assertEquals(s, "DEFINED")

  test("marking a triggered observation Inactive withdraws the trigger"):
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      _          <- setState(oid, ObservationWorkflowState.Ready)
      _          <- setState(oid, ObservationWorkflowState.Inactive)
      ts         <- triggers(oid)
      s          <- state(pid, oid)
    yield
      assertEquals(ts, List(("WITHDRAWN", None)))
      assertEquals(s, "INACTIVE")

  test("re-triggering after a withdrawal creates a second trigger, keeping the first as history"):
    for
      (_, oid) <- createTooObservationAs(pi, staff)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      _        <- setState(oid, ObservationWorkflowState.Defined)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      ts       <- triggers(oid)
    yield assertEquals(ts.map(_._1).sorted, List("REQUESTED", "WITHDRAWN"))

  test("a non-ToO observation set Ready records no trigger"):
    for
      (pid, oid) <- createTooObservationAs(pi, staff, activation = "NONE")
      _          <- setState(oid, ObservationWorkflowState.Ready)
      ts         <- triggers(oid)
      s          <- state(pid, oid)
    yield
      assertEquals(ts, Nil)
      assertEquals(s, "READY")

  test("lowering the activation to NONE while Ready withdraws the trigger"):
    for
      (_, oid) <- createTooObservationAs(pi, staff)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      _        <- setTooActivationAs(pi, oid, "NONE")
      ts       <- triggers(oid)
    yield assertEquals(ts, List(("WITHDRAWN", None)))

  test("raising the activation while Ready requests a trigger"):
    for
      (_, oid) <- createTooObservationAs(pi, staff, activation = "NONE")
      _        <- setState(oid, ObservationWorkflowState.Ready)
      _        <- setTooActivationAs(pi, oid, "RAPID")
      ts       <- triggers(oid)
    yield assertEquals(ts, List(("REQUESTED", None)))

  test("declining records the reason and returns the observation to Defined"):
    for
      (pid, oid) <- createTooObservationAs(pi, staff)
      _          <- setState(oid, ObservationWorkflowState.Ready)
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
      s          <- state(pid, oid)
    yield
      // Declined, not withdrawn: the reason survives the user-state clear.
      assertEquals(ts, List(("DECLINED", Some("weathered out"))))
      assertEquals(s, "DEFINED")

  test("a declined trigger does not block a fresh request"):
    for
      (_, oid) <- createTooObservationAs(pi, staff)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      rid      <- triggerId(oid)
      _        <- query(staff, declineQuery(rid))
      _        <- setState(oid, ObservationWorkflowState.Ready)
      ts       <- triggers(oid)
    yield assertEquals(ts.map(_._1).sorted, List("DECLINED", "REQUESTED"))

  test("a PI cannot decline"):
    for
      (_, oid) <- createTooObservationAs(pi, staff)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      rid      <- triggerId(oid)
      _        <- expect(
                    pi,
                    declineQuery(rid),
                    expected = List(s"User ${pi.id} is not authorized to perform this operation.").asLeft
                  )
    yield ()

  test("an already-declined trigger cannot be declined again"):
    for
      (_, oid) <- createTooObservationAs(pi, staff)
      _        <- setState(oid, ObservationWorkflowState.Ready)
      rid      <- triggerId(oid)
      _        <- query(staff, declineQuery(rid))
      _        <- expect(
                    staff,
                    declineQuery(rid),
                    expected = List(s"TooTrigger $rid could not be declined (not found, or no longer requested).").asLeft
                  )
    yield ()

  test("an opportunity target with NONE activation is Undefined"):
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createOpportunityTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      // The fixture helper gives an opportunity asterism a coherent activation;
      // put it back to NONE, which is the incoherence under test here.
      _   <- setTooActivationAs(pi, oid, "NONE")
      s   <- state(pid, oid)
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
      assertEquals(s, "UNDEFINED")
      assert(ms.exists(_.contains("must set a ToO activation")), s"expected the activation message, got $ms")

  test("an observation still holding an opportunity placeholder cannot be triggered"):
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createOpportunityTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- state(pid, oid)
      // Defined -> Ready excludes opportunity asterisms, so there is no way to
      // request a trigger while the placeholder is still standing in.
      r   <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready).attempt
      ts  <- triggers(oid)
    yield
      assert(r.isLeft, s"expected the transition to be refused, got $r")
      assertEquals(ts, Nil)
