// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Decoder
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.enums.TooActivation
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.core.util.Timestamp
import lucuma.odb.data.TooTrigger
import lucuma.odb.data.TooTriggerStatus
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

  def setSchedulingModeAs(user: User, oid: Observation.Id, mode: SchedulingMode): IO[Unit] =
    query(
      user,
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
    ).void

  /** The same mutation as a query string, for tests that expect it to be refused. */
  def schedulingModeQuery(oid: Observation.Id, mode: SchedulingMode): String =
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
  def tooWorkflowStateAndTransitions(pid: Program.Id, oid: Observation.Id, user: User): IO[(ObservationWorkflowState, List[ObservationWorkflowState])] =
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
      (
        c.downField("state").require[ObservationWorkflowState],
        c.downField("validTransitions").require[List[ObservationWorkflowState]]
      )

  /** Recomputes obscalc, then reads the cached workflow state back. */
  def tooWorkflowState(pid: Program.Id, oid: Observation.Id, user: User): IO[ObservationWorkflowState] =
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
    ).map(_.hcursor.downFields("observation", "workflow", "value", "state").require[ObservationWorkflowState])

  /**
   * A program with an accepted proposal and one valid *ordinary* observation,
   * sitting in `Defined` and allowed to go `Ready`.  Not a Target of
   * Opportunity: its asterism holds a sidereal target, so it derives no
   * activation and setting it `Ready` records no trigger.
   */
  def createTriggerableObservationAs(
    user:  User,
    staff: User
  ): IO[(Program.Id, Observation.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(user, "ToO")
      _   <- addProposal(user, pid, cfp.some, None)
      tid <- createTargetWithProfileAs(user, pid)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      _   <- createConfigurationRequestAs(user, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(user, oid)
      _   <- addPartnerSplits(user, pid)
      _   <- addCoisAs(user, pid)
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
    user:     User,
    staff:    User,
    resolved: Boolean = true,
    mode:     SchedulingMode = SchedulingMode.Uninterruptible
  ): IO[(Program.Id, Observation.Id, Target.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(user, "ToO")
      _   <- addProposal(user, pid, cfp.some, None)
      tid <- createOpportunityTargetAs(user, pid)
      _   <- resolveOpportunityTargetAs(user, tid).whenA(resolved)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      _   <- createConfigurationRequestAs(user, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      _   <- computeItcResultAs(user, oid)
      _   <- setSchedulingModeAs(user, oid, mode)
      _   <- addPartnerSplits(user, pid)
      _   <- addCoisAs(user, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdateAs(tooObscalcUser, pid, oid)
    yield (pid, oid, tid)

  case class Trigger(
    id:          TooTrigger.Id,
    status:      TooTriggerStatus,
    activation:  TooActivation,
    supersedes:  Option[TooTrigger.Id],
    resolution:  Option[String],
    requestedAt: Timestamp
  )

  object Trigger:
    given Decoder[Trigger] =
      Decoder.instance: c =>
        for
          id         <- c.downField("id").as[TooTrigger.Id]
          status     <- c.downField("status").as[TooTriggerStatus]
          activation <- c.downField("tooActivation").as[TooActivation]
          s          <- c.downField("supersedes").as[Option[Json]]
          supersedes <- s.traverse(_.hcursor.downField("id").as[TooTrigger.Id])
          reason     <- c.downField("resolutionReason").as[Option[String]]
          at         <- c.downField("requestedAt").as[Timestamp]
        yield Trigger(id, status, activation, supersedes, reason, at)

  def getTooTriggersAs(user: User, oid: Observation.Id): IO[List[Trigger]] =
    query(
      user,
      s"""
        query {
          tooTriggers(WHERE: { observationId: { EQ: ${oid.asJson} } }) {
            matches {
              id
              status
              tooActivation
              supersedes { id }
              resolutionReason
              requestedAt
            }
          }
        }
      """
    ).map:
      _.hcursor.downFields("tooTriggers", "matches").require[List[Json]].map: j =>
        j.hcursor.require[Trigger]

  /** The live request. */
  def getRequestedTooTriggerAs(user: User, oid: Observation.Id): IO[Trigger] =
    query(
      user,
      s"""
        query {
          tooTriggers(WHERE: { observationId: { EQ: ${oid.asJson} }, status: { EQ: REQUESTED } }) {
            matches {
              id
              status
              tooActivation
              supersedes { id }
              resolutionReason
              requestedAt
            }
          }
        }
      """
    ).map: js =>
      js.hcursor.downFields("tooTriggers", "matches").require[List[Json]].head.hcursor.require[Trigger]

  /**
   * A timing window, reduced to what the default-window rule turns on: whether
   * it includes or excludes, when it opens, and when (if ever) it closes.  Only
   * the `at` flavour of end is read, since that is the one the default uses.
   */
  case class Window(inclusion: TimingWindowInclusion, start: Timestamp, end: Option[Timestamp])

  object Window:
    given Decoder[Window] =
      Decoder.instance: c =>
        for
          i <- c.downField("inclusion").as[TimingWindowInclusion]
          s <- c.downField("startUtc").as[Timestamp]
          e <- c.downField("end").as[Option[Json]]
          a <- e.flatTraverse(_.hcursor.downField("atUtc").as[Option[Timestamp]])
        yield Window(i, s, a)

  def getTimingWindowsAs(user: User, oid: Observation.Id): IO[List[Window]] =
    query(
      user,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            schedulingConstraints {
              timingWindows {
                inclusion
                startUtc
                end { ... on TimingWindowEndAt { atUtc } }
              }
            }
          }
        }
      """
    ).map:
      _.hcursor.downFields("observation", "schedulingConstraints", "timingWindows")
       .require[List[Json]]
       .map(_.hcursor.require[Window])

  /** Replaces the observation's timing windows wholesale; `twis` is a GraphQL list literal. */
  def setTimingWindowsAs(user: User, oid: Observation.Id, twis: String): IO[Unit] =
    query(
      user,
      s"""
        mutation {
          updateObservations(input: {
            SET: { schedulingConstraints: { timingWindows: $twis } }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  /** Gives the observation one open-ended INCLUDE window of its own. */
  def setTimingWindowAs(user: User, oid: Observation.Id, startUtc: String): IO[Unit] =
    setTimingWindowsAs(user, oid, s"""[ { inclusion: INCLUDE, startUtc: "$startUtc" } ]""")

  /** Removes every timing window, leaving the observation with none. */
  def clearTimingWindowsAs(user: User, oid: Observation.Id): IO[Unit] =
    setTimingWindowsAs(user, oid, "[]")

  def declineQuery(rid: TooTrigger.Id, reason: Option[String] = None): String =
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

  def declineTooTrigger(user: User, rid: TooTrigger.Id, reason: Option[String] = None): IO[Unit] =
    query(
      user  = user,
      query = declineQuery(rid, reason)
    ).void
}
