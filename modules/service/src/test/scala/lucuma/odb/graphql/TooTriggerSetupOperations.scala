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
import lucuma.odb.TestCoordinates.coords
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

  def setTooActivationAs(user: User, oid: Observation.Id, activation: TooActivation): IO[Unit] =
    query(user, tooActivationQuery(oid, activation)).void

  /** The same mutation as a query string, for tests that expect it to be refused. */
  def tooActivationQuery(oid: Observation.Id, activation: TooActivation): String =
    s"""
      mutation {
        updateObservations(input: {
          SET: { schedulingConstraints: { tooActivation: ${activation.tag.toScreamingSnakeCase} } }
          WHERE: { id: { EQ: ${oid.asJson} } }
        }) {
          observations { id }
        }
      }
    """

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

  /** Sets the observation's workflow state, which is how a trigger is requested and withdrawn. */
  def setTooWorkflowState(user: User, oid: Observation.Id, state: ObservationWorkflowState): IO[Unit] =
    setObservationWorkflowState(user, oid, state).void

  /**
   * Swaps the opportunity placeholder out of the asterism for a real sidereal target,
   * as the alert would.  Returns the new target's id.
   *
   * The declination is inside the region `createOpportunityTargetAs` draws (10 to 70
   * degrees), which matters more than it looks: a configuration request made while the
   * observation still held its placeholder is approved against that *region*, and
   * `Configuration.subsumes` keeps the approval only while the swapped-in coordinates
   * fall inside it.  Swapping in a target outside the region is therefore not a neutral
   * choice of test data -- it makes the observation `Unapproved`.
   */
  def swapInRealTargetAs(user: User, pid: Program.Id, oid: Observation.Id, tooTid: Target.Id): IO[Target.Id] =
    swapInRealTargetAs(user, pid, oid, tooTid, "30:00:00.00")

  // Coordinates.fromHmsDms wants an explicit sign on the declination; the callers
  // write it the way it reads on the sky.
  private def signed(dec: String): String =
    if dec.startsWith("+") || dec.startsWith("-") then dec else s"+$dec"

  def swapInRealTargetAs(user: User, pid: Program.Id, oid: Observation.Id, tooTid: Target.Id, dec: String): IO[Target.Id] =
    for
      tid <- createSiderealTargetAtAs(user, pid, coords(s"05:46:13.137 ${signed(dec)}"), "Alert")
      _   <- editAsterismAs(user, oid, add = List(tid), del = List(tooTid))
    yield tid


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
   * sitting in `Defined`.  What makes it a ToO is the declared `activation`; the
   * asterism has no say in it.
   *
   * `swapped` decides whether the alert has already arrived.  Only a swapped one
   * may be triggered: while the placeholder is still in the asterism there are no
   * coordinates to point at.
   *
   * The activation is declared before the proposal is accepted on purpose: that is
   * when the proposal's ToO ceiling is fixed, at the highest activation among its
   * observations.  Declaring it afterwards would leave the ceiling at `None` and
   * the observation `Unapproved`.  Tests that move between the ToO activations
   * should create at the highest one they need and lower it from there.
   */
  def createTooObservationAs(
    user:       User,
    staff:      User,
    swapped:    Boolean = true,
    activation: TooActivation = TooActivation.Rapid
  ): IO[(Program.Id, Observation.Id, Target.Id)] =
    for
      cfp <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(user, "ToO")
      _   <- addProposal(user, pid, cfp.some, "queue: { considerForBand3: DO_NOT_CONSIDER }".some)
      tid <- createOpportunityTargetAs(user, pid)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      // A Rapid or Interrupting activation brings Uninterruptible with it.
      _   <- setTooActivationAs(user, oid, activation)
      _   <- createConfigurationRequestAs(user, oid).flatMap(setConfigurationRequestStatusAs(staff, _, ConfigurationRequestStatus.Approved))
      real <- if swapped then swapInRealTargetAs(user, pid, oid, tid).map(_.some) else none.pure[IO]
      _   <- computeItcResultAs(user, oid)
      _   <- addPartnerSplits(user, pid)
      _   <- addCoisAs(user, pid)
      _   <- setProposalStatus(staff, pid, "ACCEPTED")
      _   <- runObscalcUpdateAs(tooObscalcUser, pid, oid)
    yield (pid, oid, real.getOrElse(tid))

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
