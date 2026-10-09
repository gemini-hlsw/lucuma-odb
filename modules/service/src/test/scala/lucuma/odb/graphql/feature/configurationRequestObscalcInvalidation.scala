// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.all.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.core.model.ObservationValidation
import lucuma.core.model.ObservationWorkflow
import lucuma.core.model.Program
import lucuma.core.util.CalculationState
import lucuma.core.util.Timestamp
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.text.text
import skunk.syntax.all.*

// Configuration requests only affect workflows once the proposal is accepted,
// so changing them before then (submitting inserts them, withdrawing deletes
// them) must not recalculate the program's observations.  Acceptance itself
// recalculates everything, by way of the program reference it assigns.
class configurationRequestObscalcInvalidation extends ExecutionTestSupportForGmos:

  override val httpRequestHandler = invitationEmailRequestHandler

  private val ProgramObscalc: Query[Program.Id, (Observation.Id, CalculationState, Timestamp)] =
    sql"""
      SELECT c_observation_id, c_obscalc_state, c_last_invalidation
        FROM t_obscalc
       WHERE c_program_id = $program_id
       ORDER BY c_observation_id
    """.query(observation_id *: calculation_state *: core_timestamp)

  private def programObscalc(pid: Program.Id): IO[List[(Observation.Id, CalculationState, Timestamp)]] =
    withSession(_.execute(ProgramObscalc)(pid))

  // Marks every observation in the program Ready, with its invalidation moved
  // into the past so that a new invalidation is detectable within the test.
  private def settle(pid: Program.Id): IO[List[(Observation.Id, CalculationState, Timestamp)]] =
    withSession: session =>
      session.execute(sql"""
        UPDATE t_obscalc
           SET c_obscalc_state     = 'ready',
               c_last_invalidation = now() - interval '1 day'
         WHERE c_program_id = $program_id
      """.command)(pid).void
    *> programObscalc(pid)

  private def recalculate(pid: Program.Id): IO[Unit] =
    programObscalc(pid).flatMap(_.traverse_((oid, _, _) => runObscalcUpdate(pid, oid)))

  private def requestIds(pid: Program.Id): IO[List[ConfigurationRequest.Id]] =
    withSession: session =>
      session.execute(sql"""
        SELECT c_configuration_request_id
          FROM t_configuration_request
         WHERE c_program_id = $program_id
      """.query(configuration_request_id))(pid)

  // A submittable program with one GMOS North long slit observation.
  private val submittable: IO[(Program.Id, Observation.Id)] =
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      oid <- addDefinedObservationAs(pi, pid).map(_._2)
    yield (pid, oid)

  private def workflow(oid: Observation.Id): IO[ObservationWorkflow] =
    query(
      pi,
      s"""
        query {
          observation(observationId: "$oid") {
            workflow { value { state validTransitions validationErrors { code messages } } }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "workflow", "value").require[ObservationWorkflow])

  test("submitting does not invalidate obscalc"):
    for
      (pid, _) <- submittable
      before   <- settle(pid)
      _        <- setProposalStatus(pi, pid, "SUBMITTED")
      rids     <- requestIds(pid)
      after    <- programObscalc(pid)
    yield
      assert(rids.nonEmpty, "submission should create configuration requests")
      assertEquals(after, before)

  test("withdrawing does not invalidate obscalc"):
    for
      (pid, _) <- submittable
      _        <- setProposalStatus(pi, pid, "SUBMITTED")
      before   <- settle(pid)
      _        <- setProposalStatus(pi, pid, "NOT_SUBMITTED")
      rids     <- requestIds(pid)
      after    <- programObscalc(pid)
    yield
      assert(rids.isEmpty, "withdrawal should delete configuration requests")
      assertEquals(after, before)

  test("accepting invalidates obscalc and recalculates with configuration request checks"):
    for
      (pid, oid) <- submittable
      _          <- setProposalStatus(pi, pid, "SUBMITTED")
      _          <- recalculate(pid)
      _          <- assertIO(workflow(oid).map(_.state), ObservationWorkflowState.Defined)
      before     <- settle(pid)
      _          <- setProposalStatus(staff, pid, "ACCEPTED")
      after      <- programObscalc(pid)
      _           = assertEquals(after.map(_._2), before.as(CalculationState.Pending))
      _           = assert(after.zip(before).forall((a, b) => a._3 > b._3))
      _          <- recalculate(pid)
      wf         <- workflow(oid)
    yield
      assertEquals(wf.state, ObservationWorkflowState.Unapproved)
      assertEquals(wf.validationErrors, List(ObservationValidation.configurationRequestPending))

  private def requestIdFor(pid: Program.Id, mode: String): IO[ConfigurationRequest.Id] =
    withSession: session =>
      session.unique(sql"""
        SELECT c_configuration_request_id
          FROM t_configuration_request
         WHERE c_program_id          = $program_id
           AND c_observing_mode_type = ${text}::e_observing_mode_type
      """.query(configuration_request_id))(pid, mode)

  // Staff may move an accepted proposal back to either status.  Leaving
  // Accepted clears the program reference, which recalculates everything
  // without the configuration request checks.
  //
  // Moving to Submitted re-runs the submission rules against the workflows as
  // they stand while still accepted, and those call for a Defined observation.
  // So the GMOS North request is approved, keeping that observation Defined,
  // while the GMOS South one is left pending.  The South observation is the one
  // that shows the checks have gone.
  List("SUBMITTED", "NOT_SUBMITTED").foreach: status =>
    test(s"leaving accepted for $status invalidates obscalc and recalculates without configuration request checks"):
      for
        (pid, _) <- submittable
        tid      <- createTargetWithProfileAs(pi, pid)
        south    <- createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
        _        <- computeItcResultAs(pi, south)
        _        <- setProposalStatus(pi, pid, "SUBMITTED")
        _        <- setProposalStatus(staff, pid, "ACCEPTED")
        rid      <- requestIdFor(pid, "gmos_north_long_slit")
        _        <- setConfigurationRequestStatusAs(staff, rid, ConfigurationRequestStatus.Approved)
        _        <- recalculate(pid)
        _        <- assertIO(workflow(south).map(_.state), ObservationWorkflowState.Unapproved)
        before   <- settle(pid)
        _        <- setProposalStatus(staff, pid, status)
        after    <- programObscalc(pid)
        _         = assertEquals(after.map(_._2), before.as(CalculationState.Pending))
        _         = assert(after.zip(before).forall((a, b) => a._3 > b._3))
        _        <- recalculate(pid)
        wf       <- workflow(south)
      yield
        assertEquals(wf.state, ObservationWorkflowState.Defined)
        assertEquals(wf.validationErrors, Nil)

  test("changing a request on an accepted proposal invalidates obscalc"):
    for
      (pid, oid) <- submittable
      _          <- setProposalStatus(pi, pid, "SUBMITTED")
      _          <- setProposalStatus(staff, pid, "ACCEPTED")
      _          <- recalculate(pid)
      _          <- assertIO(workflow(oid).map(_.state), ObservationWorkflowState.Unapproved)
      before     <- settle(pid)
      rids       <- requestIds(pid)
      _          <- rids.traverse_(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      after      <- programObscalc(pid)
      _           = assertEquals(after.map(_._2), before.as(CalculationState.Pending))
      _          <- recalculate(pid)
      wf         <- workflow(oid)
    yield
      assertEquals(wf.state, ObservationWorkflowState.Defined)
      assert(wf.validTransitions.contains(ObservationWorkflowState.Ready))
