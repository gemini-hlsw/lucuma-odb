// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
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

  def setSchedulingModeAs(user: User, oid: Observation.Id, mode: String): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { schedulingMode: $mode } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Adds or removes asterism members, which is what makes an observation a ToO or stops it being one. */
  def editAsterismAs(user: User, oid: Observation.Id, add: List[Target.Id], del: List[Target.Id]): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateAsterisms(input: {
            SET: {
              ${if add.isEmpty then "" else s"ADD: [ ${add.map(t => s"\"$t\"").mkString(",")} ]"}
              ${if del.isEmpty then "" else s"DELETE: [ ${del.map(t => s"\"$t\"").mkString(",")} ]"}
            }
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

  /**
   * Gives an opportunity target sidereal coordinates, as the alert would.
   *
   * The declination is inside the region `createOpportunityTargetAs` draws (10 to 70
   * degrees), which matters more than it looks: a configuration request for an
   * unresolved ToO is approved against its *region*, and `Configuration.subsumes` keeps
   * that approval only while the resolved coordinates fall inside it.  Resolving
   * outside the region is therefore not a neutral choice of test data -- it makes the
   * observation `Unapproved`, which is pinned by tooTriggerWorkflow.
   *
   * The region is deliberately not restated: omitting it leaves the approved region
   * exactly as it was, which is the whole point of resolving being safe.
   */
  def resolveOpportunityTargetAs(user: User, tid: Target.Id): IO[Unit] =
    resolveOpportunityTargetAs(user, tid, "30:00:00.00")

  def resolveOpportunityTargetAs(user: User, tid: Target.Id, dec: String): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateTargets(input: {
            SET: {
              opportunity: {
                resolution: {
                  sidereal: { ra: { hms: "05:46:13.137" }, dec: { dms: "$dec" }, epoch: "J2000.0" }
                }
              }
            }
            WHERE: { id: { EQ: ${tid.asJson} } }
          }) {
            targets { id }
          }
        }
      """
    ).void

  /** Reads the observation's workflow state together with the transitions it is offered. */
  def tooWorkflowStateAndTransitions(pid: Program.Id, oid: Observation.Id, user: User): IO[(String, List[String])] =
    runObscalcUpdateAs(tooObscalcUser, pid, oid) *>
    query(
      user,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { state validTransitions } }
          }
        }
      """
    ).map: js =>
      val c = js.hcursor.downFields("observation", "workflow", "value")
      (c.downField("state").require[String], c.downField("validTransitions").require[List[String]])

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
   * A program with an accepted proposal and one valid *ordinary* observation,
   * sitting in `Defined` and allowed to go `Ready`.  Not a Target of
   * Opportunity: its asterism holds a sidereal target, so it derives no
   * activation and setting it `Ready` records no trigger.
   */
  def createTriggerableObservationAs(
    pi:    User,
    staff: User
  ): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(pi, oid)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdateAs(tooObscalcUser, pid, oid)
    yield (pid, oid)

  /**
   * A program with an accepted proposal and one valid Target of Opportunity,
   * sitting in `Defined`.  What makes it a ToO is the opportunity target in its
   * asterism; how disruptive it may be follows from `mode`.
   *
   * `resolved` decides whether the alert has already arrived.  Only a resolved
   * one may be triggered, since an unresolved target has no coordinates to point
   * at.
   *
   * The mode is set before the proposal is accepted on purpose: the ceiling is
   * derived from the program's observations and frozen at acceptance, so doing it
   * the other way round would freeze a lower ceiling and leave the observation
   * `Unapproved`.
   */
  def createTooObservationAs(
    pi:       User,
    staff:    User,
    resolved: Boolean = true,
    mode:     String  = "UNINTERRUPTIBLE"
  ): IO[(Program.Id, Observation.Id, Target.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi, "ToO")
      _   <- addProposal(pi, pid, cfp.some, None)
      tid <- createOpportunityTargetAs(pi, pid)
      _   <- resolveOpportunityTargetAs(pi, tid).whenA(resolved)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- createConfigurationRequestAs(pi, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(pi, oid)
      _   <- setSchedulingModeAs(pi, oid, mode)
      _   <- addPartnerSplits(pi, pid)
      _   <- addCoisAs(pi, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdateAs(tooObscalcUser, pid, oid)
    yield (pid, oid, tid)

}
