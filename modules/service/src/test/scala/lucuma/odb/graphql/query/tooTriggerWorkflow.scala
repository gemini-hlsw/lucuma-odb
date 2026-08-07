// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SequenceCommand
import lucuma.core.enums.SlewStage
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target

/**
 * The Target-of-Opportunity trigger's effect on the observation workflow: a ToO
 * observation reaches `Ready` only by having a trigger accepted, and returns to
 * `Defined` when it is withdrawn.  `Ready` is derived from the trigger rather
 * than stored, which is what these tests are really pinning down -- notably that
 * it survives an `Inactive` round trip and cannot be set by hand.
 */
class tooTriggerWorkflow extends ExecutionTestSupportForGmos:

  // temporary, until this is doable via graphql
  private def approveConfigurationRequestHack(req: ConfigurationRequest.Id): IO[Unit] =
    import skunk.syntax.all.*
    import lucuma.odb.util.Codecs.configuration_request_id
    session.use: s =>
      s.prepareR(sql"update t_configuration_request set c_status = 'approved' where c_configuration_request_id = $configuration_request_id".command).use: ps =>
        ps.execute(req).void

  private def setTooActivation(oid: Observation.Id, activation: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { tooActivation: $activation } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Recomputes obscalc, then reads back the cached workflow. */
  private def workflow(pid: Program.Id, oid: Observation.Id): IO[(String, List[String], List[String])] =
    runObscalcUpdateAs(serviceUser, pid, oid) *>
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow {
              value {
                state
                validTransitions
                validationErrors { messages }
              }
            }
          }
        }
      """
    ).map: json =>
      val c = json.hcursor.downFields("observation", "workflow", "value")
      (
        c.downField("state").require[String],
        c.downField("validTransitions").require[List[String]],
        c.downField("validationErrors").require[List[io.circe.Json]]
          .flatMap(_.hcursor.downField("messages").require[List[String]])
      )

  private def state(pid: Program.Id, oid: Observation.Id): IO[String] =
    workflow(pid, oid).map(_._1)

  private def requestTrigger(oid: Observation.Id): IO[String] =
    query(
      pi,
      s"""
        mutation {
          requestTooTrigger(input: { observationId: ${oid.asJson} }) {
            tooTrigger { id }
          }
        }
      """
    ).map(_.hcursor.downFields("requestTooTrigger", "tooTrigger", "id").require[String])

  private def acceptTrigger(rid: String): IO[String] =
    query(
      staff,
      s"""
        mutation {
          acceptTooTrigger(input: { tooTriggerId: "$rid" }) {
            tooTrigger { status }
          }
        }
      """
    ).map(_.hcursor.downFields("acceptTooTrigger", "tooTrigger", "status").require[String])

  private def withdrawTriggerQuery(rid: String): String =
    s"""
      mutation {
        withdrawTooTrigger(input: { tooTriggerId: "$rid" }) {
          tooTrigger { status }
        }
      }
    """

  private def withdrawTrigger(rid: String): IO[String] =
    query(pi, withdrawTriggerQuery(rid))
      .map(_.hcursor.downFields("withdrawTooTrigger", "tooTrigger", "status").require[String])

  /**
   * A program with an accepted-shaped proposal and one fully valid GMOS North
   * observation, left in `Defined`.  With `activation` set on the only
   * observation, the derived proposal ceiling matches it, so the ToO ceiling
   * check passes without an explicit ceiling.
   */
  private def setup(activation: String, tids: List[Target.Id] => List[Target.Id] = identity): IO[(Program.Id, Observation.Id, Target.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, tids(List(tid)))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(approveConfigurationRequestHack)
      _   <- computeItcResultAs(pi, oid)
      _   <- setTooActivation(oid, activation)
    yield (pid, oid, tid)

  private def triggered(activation: String): IO[(Program.Id, Observation.Id, String)] =
    for
      (pid, oid, _) <- setup(activation)
      _             <- state(pid, oid) // settle obscalc so the request precondition sees Defined
      rid           <- requestTrigger(oid)
      _             <- acceptTrigger(rid)
    yield (pid, oid, rid)

  test("a ToO observation with no trigger is Defined and offers no Ready transition"):
    for
      (pid, oid, _) <- setup("RAPID")
      (s, ts, _)    <- workflow(pid, oid)
    yield
      assertEquals(s, "DEFINED")
      assertEquals(ts, List("INACTIVE"))

  test("an accepted trigger makes the observation Ready"):
    for
      (pid, oid, _) <- triggered("RAPID")
      (s, ts, _)    <- workflow(pid, oid)
    yield
      assertEquals(s, "READY")
      // No un-ready transition: withdrawing the trigger is the lever.
      assertEquals(ts, List("INACTIVE"))

  test("withdrawing an accepted trigger returns the observation to Defined"):
    for
      (pid, oid, rid) <- triggered("RAPID")
      _               <- workflow(pid, oid)
      st              <- withdrawTrigger(rid)
      s               <- state(pid, oid)
    yield
      assertEquals(st, "WITHDRAWN")
      assertEquals(s, "DEFINED")

  test("a requested but unaccepted trigger leaves the observation Defined"):
    for
      (pid, oid, _) <- setup("RAPID")
      _             <- state(pid, oid)
      _             <- requestTrigger(oid)
      s             <- state(pid, oid)
    yield assertEquals(s, "DEFINED")

  test("Ready survives an Inactive round trip, because it is derived from the trigger"):
    for
      (pid, oid, _) <- triggered("RAPID")
      _             <- workflow(pid, oid)
      _             <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Inactive)
      inactive      <- state(pid, oid)
      _             <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready)
      restored      <- state(pid, oid)
    yield
      assertEquals(inactive, "INACTIVE")
      assertEquals(restored, "READY")

  test("Ready cannot be set by hand on a ToO observation"):
    for
      (pid, oid, _) <- setup("RAPID")
      _             <- state(pid, oid)
      r             <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Ready).attempt
    yield assert(r.isLeft, s"expected the transition to be refused, got $r")

  test("an opportunity target with NONE activation is Undefined"):
    for
      cfp        <- createGeminiCallForProposalsAs(staff)
      pid        <- createProgramAs(pi, "ToO")
      _          <- addProposal(pi, pid, cfp.some, None)
      tid        <- createOpportunityTargetAs(pi, pid)
      oid        <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      // The fixture helper gives an opportunity asterism a coherent activation;
      // put it back to NONE, which is the incoherence under test here.
      _          <- setTooActivation(oid, "NONE")
      (s, _, ms) <- workflow(pid, oid)
    yield
      assertEquals(s, "UNDEFINED")
      assert(
        ms.exists(_.contains("must set a ToO activation")),
        s"expected the activation message, got $ms"
      )

  test("swapping an opportunity target back in after acceptance drops the observation to Undefined"):
    for
      (pid, oid, tid) <- setup("RAPID")
      _               <- state(pid, oid)
      rid             <- requestTrigger(oid)
      _               <- acceptTrigger(rid)
      ready           <- state(pid, oid)
      opp             <- createOpportunityTargetAs(pi, pid)
      _               <- updateAsterisms(pi, List(oid), List(opp), List(tid), List((oid, List(opp))))
      (s, _, ms)      <- workflow(pid, oid)
    yield
      assertEquals(ready, "READY")
      assertEquals(s, "UNDEFINED")
      assert(
        ms.exists(_.contains("Replace the Target of Opportunity placeholder")),
        s"expected the unresolved-placeholder message, got $ms"
      )

  test("a trigger cannot be withdrawn once execution has begun"):
    for
      (pid, oid, rid) <- triggered("RAPID")
      _               <- workflow(pid, oid)
      vid             <- recordVisitAs(serviceUser, oid)
      _               <- addSequenceEventAs(serviceUser, vid, SequenceCommand.Start)
      _               <- expect(
                           pi,
                           withdrawTriggerQuery(rid),
                           expected = List(
                             s"TooTrigger $rid could not be withdrawn (not found, not writable, already resolved, or its observation has begun executing)."
                           ).asLeft
                         )
    yield ()

  test("a slew alone does not block withdrawal"):
    for
      (pid, oid, rid) <- triggered("RAPID")
      _               <- workflow(pid, oid)
      _               <- addSlewEventAs(serviceUser, oid, SlewStage.StartSlew)
      st              <- withdrawTrigger(rid)
    yield assertEquals(st, "WITHDRAWN")

  test("a request is refused when the activation is NONE"):
    for
      (pid, oid, _) <- setup("NONE")
      _             <- state(pid, oid)
      _             <- expect(
                         pi,
                         s"""mutation { requestTooTrigger(input: { observationId: ${oid.asJson} }) { tooTrigger { id } } }""",
                         expected = List(
                           "This observation is not a Target of Opportunity; set its ToO activation before requesting a trigger."
                         ).asLeft
                       )
    yield ()

  test("a request is refused while an opportunity placeholder remains"):
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramAs(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createOpportunityTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setTooActivation(oid, "RAPID")
      _   <- state(pid, oid)
      _   <- expect(
               pi,
               s"""mutation { requestTooTrigger(input: { observationId: ${oid.asJson} }) { tooTrigger { id } } }""",
               expected = List(
                 "This observation still has a Target of Opportunity placeholder; replace it with the actual target coordinates before requesting a trigger."
               ).asLeft
             )
    yield ()

  test("a request is refused when the observation is not Defined"):
    for
      (pid, oid, _) <- setup("RAPID")
      _             <- state(pid, oid)
      _             <- setObservationWorkflowState(pi, oid, ObservationWorkflowState.Inactive)
      _             <- state(pid, oid)
      _             <- expect(
                         pi,
                         s"""mutation { requestTooTrigger(input: { observationId: ${oid.asJson} }) { tooTrigger { id } } }""",
                         expected = List(
                           "A ToO trigger may only be requested for an observation in the Defined state, but this one is INACTIVE."
                         ).asLeft
                       )
    yield ()

  test("a second request is refused while one is already live"):
    for
      (pid, oid, _) <- setup("RAPID")
      _             <- state(pid, oid)
      _             <- requestTrigger(oid)
      _             <- state(pid, oid)
      _             <- expect(
                         pi,
                         s"""mutation { requestTooTrigger(input: { observationId: ${oid.asJson} }) { tooTrigger { id } } }""",
                         expected = List(
                           "This observation already has a live (requested or accepted) ToO trigger."
                         ).asLeft
                       )
    yield ()

  test("a withdrawn trigger does not block a fresh request"):
    for
      (pid, oid, _) <- setup("RAPID")
      _             <- state(pid, oid)
      rid           <- requestTrigger(oid)
      _             <- withdrawTrigger(rid)
      _             <- state(pid, oid)
      rid2          <- requestTrigger(oid)
    yield assertNotEquals(rid, rid2)
