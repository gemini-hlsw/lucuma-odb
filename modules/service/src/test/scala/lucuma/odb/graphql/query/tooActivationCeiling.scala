// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

/**
 * The Target-of-Opportunity ceiling: an unset proposal-level activation is
 * derived as the maximum among the program's observations, frozen on
 * acceptance, and thereafter enforced against each observation.
 */
class tooActivationCeiling extends OdbSuite with TooTriggerSetupOperations:

  val pi:      User = TestUsers.Standard.pi(1, 101)
  val staff:   User = TestUsers.Standard.staff(3, 103)
  val service      = TestUsers.service(4)

  override val validUsers: List[User] = List(pi, staff)

  /**
   * Makes an observation derive `activation`.  The activation is no longer set
   * directly: an observation is a Target of Opportunity exactly when its asterism
   * holds an opportunity target, and how disruptive it may be follows from its
   * scheduling mode.  So this adds the target and picks the mode that produces
   * the wanted value.
   */
  private def deriveTooActivation(pid: Program.Id, oid: Observation.Id, activation: String): IO[Unit] =
    val mode = activation match
      case "STANDARD"     => "UNCONSTRAINED"
      case "RAPID"        => "UNINTERRUPTIBLE"
      case "INTERRUPTING" => "INTERRUPTING"
      case other          => fail(s"no scheduling mode derives $other")
    for
      tid <- createOpportunityTargetAs(pi, pid)
      _   <- resolveOpportunityTargetAs(pi, tid)
      _   <- addOpportunityTargetToAsterism(oid, tid)
      _   <- setSchedulingMode(oid, mode)
    yield ()

  private def addOpportunityTargetToAsterism(oid: Observation.Id, tid: Target.Id): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateAsterisms(input: {
            SET: { ADD: [ ${tid.asJson} ] }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """
    ).void

  private def setSchedulingMode(oid: Observation.Id, mode: String): IO[Unit] =
    query(
      pi,
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

  /** Sets the explicit ceiling, which after acceptance is what enforcement uses. */
  private def setExplicitCeiling(pid: Program.Id, ceiling: String): IO[Unit] =
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

  private def setSchedulingModeQuery(oid: Observation.Id, mode: String): String =
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

  private def schedulingMode(oid: Observation.Id): IO[String] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            schedulingConstraints { schedulingMode }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "schedulingConstraints", "schedulingMode").require[String])

  private def proposalCeiling(pid: Program.Id): IO[String] =
    query(
      pi,
      s"""
        query {
          program(programId: ${pid.asJson}) {
            proposal { gemini { ... on Queue { tooActivationCeiling } } }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("program", "proposal", "gemini", "tooActivationCeiling")
       .require[String]

  /** The explicit ceiling, which is null until acceptance freezes it. */
  private def proposalExplicitCeiling(pid: Program.Id): IO[Option[String]] =
    query(
      pi,
      s"""
        query {
          program(programId: ${pid.asJson}) {
            proposal { gemini { ... on Queue { explicitTooActivationCeiling } } }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("program", "proposal", "gemini", "explicitTooActivationCeiling")
       .require[Option[String]]

  private def validationCodes(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { state validationErrors { code } } }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("observation", "workflow", "value", "validationErrors")
       .values
       .toList
       .flatten
       .flatMap(_.hcursor.downField("code").as[String].toOption)

  private def workflowState(oid: Observation.Id): IO[String] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { state } }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "workflow", "value", "state").require[String])

  /** A program with a proposal that leaves tooActivation unset, and one observation. */
  private def setup(activation: String): IO[(Program.Id, Observation.Id)] =
    for
      _ <- createUsers(pi, staff)
      c <- createGeminiCallForProposalsAs(staff, RegularSemester)
      p <- createProgramWithNonPartnerPi(pi, "ToO ceiling")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- deriveTooActivation(p, o, activation)
      _ <- computeItcResultAs(pi, o)
      _ <- addProposal(pi, p, c.some)
      _ <- addPartnerSplits(pi, p)
      _ <- addCoisAs(pi, p)
    yield (p, o)

  test("an unset ceiling is derived from the program's observations"):
    for
      (p, o) <- setup("RAPID")
      too    <- proposalCeiling(p)
    yield assertEquals(too, "RAPID")

  test("the derived ceiling tracks the maximum, not the first"):
    for
      (p, o) <- setup("STANDARD")
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _      <- deriveTooActivation(p, o2, "INTERRUPTING")
      too    <- proposalCeiling(p)
    yield assertEquals(too, "INTERRUPTING")

  test("the ceiling is derived, not explicit, until the proposal is accepted"):
    for
      (p, o) <- setup("RAPID")
      before <- proposalExplicitCeiling(p)
      _      <- acceptProposal(staff, p)
      after  <- proposalExplicitCeiling(p)
    yield
      assertEquals(before, None)            // derived from the observations
      assertEquals(after,  "RAPID".some)    // frozen into the explicit field

  test("acceptance freezes the ceiling, so a later observation cannot raise it"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      before <- proposalCeiling(p)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      // The mode goes first, while the asterism is still ordinary: the activation
      // derives NONE, so the ceiling guard has nothing to object to.  Setting the
      // mode after the opportunity target were added would be refused outright --
      // which is the point of the guard, and why this reaches INTERRUPTING through
      // the asterism instead.
      _      <- setSchedulingMode(o2, "INTERRUPTING")
      tid    <- createOpportunityTargetAs(pi, p)
      _      <- resolveOpportunityTargetAs(pi, tid)
      _      <- addOpportunityTargetToAsterism(o2, tid)
      after  <- proposalCeiling(p)
    yield
      assertEquals(before, "RAPID")
      assertEquals(after,  "RAPID") // frozen: the new observation does not raise its own ceiling

  // Raising the mode over the ceiling is refused outright now, so the only way
  // to reach this state is for the ceiling to move beneath a settled observation.
  // The validator still matters: the ceiling is not the only thing that can move.
  test("an observation exceeding the frozen ceiling is flagged and cannot become ready"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      _      <- setExplicitCeiling(p, "STANDARD")
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
      state  <- workflowState(o)
    yield
      assert(codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"expected ceiling violation, got $codes")
      assertEquals(state, "UNAPPROVED")

  test("an observation at or below the frozen ceiling is not flagged"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      _      <- setSchedulingMode(o, "UNCONSTRAINED")
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
    yield assert(!codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"unexpected ceiling violation: $codes")

  test("a PI may lower the mode after the ceiling moves beneath them, recovering the observation"):
    for
      (p, o)  <- setup("RAPID")
      _       <- acceptProposal(staff, p)
      _       <- setExplicitCeiling(p, "STANDARD")
      _       <- runObscalcUpdateAs(service, p, o)
      before  <- validationCodes(o)
      state   <- workflowState(o)
      // UNAPPROVED is a pre-execution state, so the observation is still editable
      // and the PI is not stranded above a ceiling they cannot come back under.
      // It has to come all the way down to a compliant mode: while the derived
      // activation is over the ceiling, any mode leaving it over is refused.
      _       <- setSchedulingMode(o, "UNCONSTRAINED")
      _       <- runObscalcUpdateAs(service, p, o)
      after   <- validationCodes(o)
    yield
      assertEquals(state, "UNAPPROVED")
      assert(before.contains("TOO_ACTIVATION_UNAPPROVED"), s"expected ceiling violation, got $before")
      // The ceiling violation clears.  This fixture has no approved configuration,
      // so the observation stays UNAPPROVED for that unrelated reason -- what is
      // being pinned is that the ceiling no longer contributes.
      assert(!after.contains("TOO_ACTIVATION_UNAPPROVED"), s"expected recovery, got $after")

  // -- The guard on direct mode edits ---------------------------------------

  private def ceilingRefusal(oid: Observation.Id, activation: String, ceiling: String) =
    List(
      s"Cannot set the scheduling mode for observation $oid: Target of Opportunity activation $activation exceeds the maximum $ceiling allowed by the proposal."
    ).asLeft

  test("raising the mode above the frozen ceiling is refused"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      _      <- expect(pi, setSchedulingModeQuery(o, "INTERRUPTING"), ceilingRefusal(o, "INTERRUPTING", "RAPID"))
    yield ()

  test("the refusal leaves the observation untouched"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      before <- schedulingMode(o)
      _      <- expect(pi, setSchedulingModeQuery(o, "INTERRUPTING"), ceilingRefusal(o, "INTERRUPTING", "RAPID"))
      after  <- schedulingMode(o)
    yield
      // The check runs after the update inside the transaction, so a violation
      // has to roll the whole thing back rather than leave it half applied.
      assertEquals(before, "UNINTERRUPTIBLE")
      assertEquals(after, before)

  test("a mode at or below the frozen ceiling is accepted"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      _      <- setSchedulingMode(o, "UNCONSTRAINED")
      mode   <- schedulingMode(o)
    yield assertEquals(mode, "UNCONSTRAINED")

  test("the ceiling does not constrain the mode before it is frozen"):
    for
      (p, o) <- setup("RAPID")
      // No acceptance, so no explicit ceiling.  The derived one is the maximum
      // over the program's own observations, which this edit is raising -- checking
      // against it would refuse a PI for describing their own proposal.
      _      <- setSchedulingMode(o, "INTERRUPTING")
      mode   <- schedulingMode(o)
      too    <- proposalCeiling(p)
    yield
      assertEquals(mode, "INTERRUPTING")
      assertEquals(too, "INTERRUPTING")

  test("a non-ToO observation may take any mode, whatever the ceiling"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      // No opportunity target, so the activation derives NONE whatever the mode.
      // INTERRUPTING is invalid for a different reason -- it needs a ToO target --
      // but that is the workflow's business, not the ceiling's.
      _      <- setSchedulingMode(o2, "UNINTERRUPTIBLE")
      mode   <- schedulingMode(o2)
    yield assertEquals(mode, "UNINTERRUPTIBLE")
