// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.query.ObservingModeSetupOperations

/**
 * Builds an observation that can actually be triggered -- that is, one whose
 * `Defined -> Ready` transition is allowed, since setting it `Ready` is what
 * requests the trigger.  That takes more than a `createObservationAs`: the
 * observation has to be genuinely valid (real target, observing mode, ITC
 * results, approved configuration) and its proposal has to be accepted.
 */
trait TooTriggerSetupOperations extends ObservingModeSetupOperations { this: OdbSuite =>

  /** The service user used to drive obscalc; not a GraphQL caller. */
  val tooObscalcUser = TestUsers.service(97)

  def setTooActivationAs(user: User, oid: Observation.Id, activation: String): IO[Unit] =
    query(
      user,
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

  /** Sets the observation's workflow state, which is how a trigger is requested and withdrawn. */
  def setTooWorkflowState(user: User, oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    setObservationWorkflowState(user, oid, state).void

  /** Recomputes obscalc, then reads the cached workflow state back. */
  def tooWorkflowState(pid: Program.Id, oid: Observation.Id, user: User): IO[String] =
    runObscalcUpdateAs(tooObscalcUser, pid, oid) *>
    query(
      user,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { state } }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "workflow", "value", "state").require[String])

  /**
   * A program with an accepted proposal and one valid ToO observation, sitting
   * in `Defined` and ready to be triggered.
   *
   * The activation is set before the proposal is accepted on purpose: the
   * ceiling is derived from the program's observations and frozen at acceptance,
   * so doing it the other way round would freeze a NONE ceiling and leave the
   * observation `Unapproved`.
   */
  def createTooObservationAs(
    pi:         User,
    staff:      User,
    activation: String = "RAPID"
  ): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(pi, oid)
      _   <- setTooActivationAs(pi, oid, activation)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdateAs(tooObscalcUser, pid, oid)
    yield (pid, oid)

}
