// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.TooActivation
import lucuma.core.enums.TooActivation.Interrupting
import lucuma.core.enums.TooActivation.Rapid
import lucuma.core.enums.TooActivation.Standard
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.string.*

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
  private def deriveTooActivation(pid: Program.Id, oid: Observation.Id, activation: TooActivation): IO[Unit] =
    val mode = activation match
      case Standard           => SchedulingMode.Unconstrained
      case Rapid              => SchedulingMode.Uninterruptible
      case Interrupting       => SchedulingMode.Interrupting
      case TooActivation.None => fail("no scheduling mode derives NONE")
    for
      tid <- createOpportunityTargetAs(pi, pid)
      _   <- resolveOpportunityTargetAs(pi, tid)
      _   <- addOpportunityTargetToAsterism(oid, tid)
      _   <- setSchedulingModeAs(pi, oid, mode)
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

  /** Sets the explicit ceiling, which after acceptance is what enforcement uses. */
  private def setExplicitCeiling(pid: Program.Id, ceiling: TooActivation): IO[Unit] =
    query(
      staff,
      s"""
        mutation {
          updateProposal(input: {
            programId: "$pid"
            SET: { gemini: { queue: { explicitTooActivationCeiling: ${ceiling.tag.toScreamingSnakeCase} } } }
          }) {
            proposal { gemini { ... on Queue { explicitTooActivationCeiling } } }
          }
        }
      """
    ).void

  private def schedulingMode(oid: Observation.Id): IO[SchedulingMode] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            schedulingConstraints { schedulingMode }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "schedulingConstraints", "schedulingMode").require[SchedulingMode])

  private def proposalCeiling(pid: Program.Id): IO[TooActivation] =
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
       .require[TooActivation]

  /** The explicit ceiling, which is null until acceptance freezes it. */
  private def proposalExplicitCeiling(pid: Program.Id): IO[Option[TooActivation]] =
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
       .require[Option[TooActivation]]

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
  private def setup(activation: TooActivation): IO[(Program.Id, Observation.Id)] =
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
      (p, o) <- setup(Rapid)
      too    <- proposalCeiling(p)
    yield assertEquals(too, Rapid)

  test("the derived ceiling tracks the maximum, not the first"):
    for
      (p, o) <- setup(Standard)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _      <- deriveTooActivation(p, o2, Interrupting)
      too    <- proposalCeiling(p)
    yield assertEquals(too, Interrupting)

  test("the ceiling is derived, not explicit, until the proposal is accepted"):
    for
      (p, o) <- setup(Rapid)
      before <- proposalExplicitCeiling(p)
      _      <- acceptProposal(staff, p)
      after  <- proposalExplicitCeiling(p)
    yield
      assertEquals(before, None)            // derived from the observations
      assertEquals(after,  Rapid.some)      // frozen into the explicit field

  test("acceptance freezes the ceiling, so a later observation cannot raise it"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      before <- proposalCeiling(p)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      // The mode goes first, while the asterism is still ordinary: the activation
      // derives NONE, so the ceiling guard has nothing to object to.  Setting the
      // mode after the opportunity target were added would be refused outright --
      // which is the point of the guard, and why this reaches INTERRUPTING through
      // the asterism instead.
      _      <- setSchedulingModeAs(pi, o2, SchedulingMode.Interrupting)
      tid    <- createOpportunityTargetAs(pi, p)
      _      <- resolveOpportunityTargetAs(pi, tid)
      _      <- addOpportunityTargetToAsterism(o2, tid)
      after  <- proposalCeiling(p)
    yield
      assertEquals(before, Rapid)
      assertEquals(after,  Rapid) // frozen: the new observation does not raise its own ceiling

  // Raising the mode over the ceiling is refused outright now, so the only way
  // to reach this state is for the ceiling to move beneath a settled observation.
  // The validator still matters: the ceiling is not the only thing that can move.
  test("an observation exceeding the frozen ceiling is flagged and cannot become ready"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      _      <- setExplicitCeiling(p, Standard)
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
      state  <- workflowState(o)
    yield
      assert(codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"expected ceiling violation, got $codes")
      assertEquals(state, "UNAPPROVED")

  test("an observation at or below the frozen ceiling is not flagged"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      _      <- setSchedulingModeAs(pi, o, SchedulingMode.Unconstrained)
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
    yield assert(!codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"unexpected ceiling violation: $codes")

  test("a PI may lower the mode after the ceiling moves beneath them, recovering the observation"):
    for
      (p, o)  <- setup(Rapid)
      _       <- acceptProposal(staff, p)
      _       <- setExplicitCeiling(p, Standard)
      _       <- runObscalcUpdateAs(service, p, o)
      before  <- validationCodes(o)
      state   <- workflowState(o)
      // UNAPPROVED is a pre-execution state, so the observation is still editable
      // and the PI is not stranded above a ceiling they cannot come back under.
      // It has to come all the way down to a compliant mode: while the derived
      // activation is over the ceiling, any mode leaving it over is refused.
      _       <- setSchedulingModeAs(pi, o, SchedulingMode.Unconstrained)
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

  private def ceilingRefusal(oid: Observation.Id, activation: TooActivation, ceiling: TooActivation) =
    List(
      s"Cannot set the scheduling mode for observation $oid: Target of Opportunity activation ${activation.tag.toScreamingSnakeCase} exceeds the maximum ${ceiling.tag.toScreamingSnakeCase} allowed by the proposal."
    ).asLeft

  test("raising the mode above the frozen ceiling is refused"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      _      <- expect(pi, schedulingModeQuery(o, SchedulingMode.Interrupting), ceilingRefusal(o, Interrupting, Rapid))
    yield ()

  test("the refusal leaves the observation untouched"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      before <- schedulingMode(o)
      _      <- expect(pi, schedulingModeQuery(o, SchedulingMode.Interrupting), ceilingRefusal(o, Interrupting, Rapid))
      after  <- schedulingMode(o)
    yield
      // The check runs after the update inside the transaction, so a violation
      // has to roll the whole thing back rather than leave it half applied.
      assertEquals(before, SchedulingMode.Uninterruptible)
      assertEquals(after, before)

  test("a mode at or below the frozen ceiling is accepted"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      _      <- setSchedulingModeAs(pi, o, SchedulingMode.Unconstrained)
      mode   <- schedulingMode(o)
    yield assertEquals(mode, SchedulingMode.Unconstrained)

  test("the ceiling does not constrain the mode before it is frozen"):
    for
      (p, o) <- setup(Rapid)
      // No acceptance, so no explicit ceiling.  The derived one is the maximum
      // over the program's own observations, which this edit is raising -- checking
      // against it would refuse a PI for describing their own proposal.
      _      <- setSchedulingModeAs(pi, o, SchedulingMode.Interrupting)
      mode   <- schedulingMode(o)
      too    <- proposalCeiling(p)
    yield
      assertEquals(mode, SchedulingMode.Interrupting)
      assertEquals(too, Interrupting)

  test("a non-ToO observation may take any mode, whatever the ceiling"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      // No opportunity target, so the activation derives NONE whatever the mode.
      // INTERRUPTING is invalid for a different reason -- it needs a ToO target --
      // but that is the workflow's business, not the ceiling's.
      _      <- setSchedulingModeAs(pi, o2, SchedulingMode.Uninterruptible)
      mode   <- schedulingMode(o2)
    yield assertEquals(mode, SchedulingMode.Uninterruptible)

  // -- Exchange proposals ----------------------------------------------------
  //
  // A Subaru proposal derives and freezes its ceiling exactly like a Gemini one.
  // It used to be capped at NONE instead, on the theory that a non-Gemini
  // observatory may not have ToOs, so every exchange ToO sat at UNAPPROVED
  // forever.  Keck is still capped: nobody has asked for ToOs there.  See V1286.

  private def validTransitions(oid: Observation.Id): IO[List[String]] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            workflow { value { validTransitions } }
          }
        }
      """
    ).map:
      _.hcursor
       .downFields("observation", "workflow", "value", "validTransitions")
       .require[List[String]]

  /** An exchange program with one exchange observation deriving `activation`. */
  private def exchangeSetup(
    mode:       ExchangeObservingModeType,
    activation: TooActivation
  ): IO[(Program.Id, Observation.Id)] =
    val (call, typeInput) = mode match
      case ExchangeObservingModeType.ExchangeSubaru => (createSubaruCallForProposalsAs(staff), "subaru")
      case ExchangeObservingModeType.ExchangeKeck   => (createKeckCallForProposalsAs(staff),   "keck")
    for
      _ <- createUsers(pi, staff)
      c <- call
      p <- createProgramWithNonPartnerPi(pi, "Exchange ToO")
      _ <- query(pi, s"""
             mutation {
               createProposal(input: {
                 programId: "$p"
                 SET: {
                   category: GALACTIC_OTHER
                   callId: "$c"
                   $typeInput: { partnerSplits: [{ partner: US, percent: 100 }] }
                 }
               }) { proposal { category } }
             }
           """)
      t <- createTargetWithProfileAs(pi, p)
      o <- createExchangeModeObservationAs(pi, p, mode, t)
      _ <- deriveTooActivation(p, o, activation)
      _ <- addCoisAs(pi, p)
    yield (p, o)

  private def subaruSetup(activation: TooActivation): IO[(Program.Id, Observation.Id)] =
    exchangeSetup(ExchangeObservingModeType.ExchangeSubaru, activation)

  // Subaru adopted our nomenclature, so an sToO is a ToO target under an
  // ordinary scheduling mode and an rToO is one under UNINTERRUPTIBLE.
  List(Standard, Rapid).foreach: activation =>
    test(s"a Subaru exchange ${activation.tag} ToO is not flagged"):
      for
        (p, o) <- subaruSetup(activation)
        _      <- runObscalcUpdateAs(service, p, o)
        codes  <- validationCodes(o)
        state  <- workflowState(o)
      yield
        assert(!codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"unexpected ceiling violation: $codes")
        assertEquals(state, "DEFINED")

  // Only Subaru asked for this.  A Keck proposal is still capped at NONE, so its
  // ToO is flagged exactly as a Subaru one used to be -- the restriction is a
  // one-line predicate change away if Keck ever asks.
  test("a Keck exchange ToO is still flagged"):
    for
      (p, o) <- exchangeSetup(ExchangeObservingModeType.ExchangeKeck, Standard)
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
      state  <- workflowState(o)
    yield
      assert(codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"expected ceiling violation, got $codes")
      assertEquals(state, "UNAPPROVED")

  // The other half of the requirement: defining one is supported, triggering it
  // is not.  Requesting a trigger is what setting an observation READY means, and
  // exchange observations have no such lifecycle -- they execute at Subaru.
  test("a Subaru exchange ToO is still never offered READY"):
    for
      (p, o) <- subaruSetup(Rapid)
      _      <- runObscalcUpdateAs(service, p, o)
      ts     <- validTransitions(o)
    yield assertEquals(ts, List("INACTIVE"))

  // The ceiling a Subaru proposal derives is the maximum over its own
  // observations, so submitting an sToO and coming back for an rToO after
  // acceptance is refused -- the same escalation, and the same refusal, a Gemini
  // PI gets.  Under the old cap the refusal was there too, but against a ceiling
  // of NONE, so it fired on the sToO itself.
  test("a Subaru exchange proposal freezes the ceiling its observations derived"):
    for
      (p, o) <- subaruSetup(Standard)
      _      <- acceptProposal(staff, p)
      _      <- expect(pi, schedulingModeQuery(o, SchedulingMode.Uninterruptible), ceilingRefusal(o, Rapid, Standard))
      // ... and everything at or below what was proposed still moves freely.
      _      <- setSchedulingModeAs(pi, o, SchedulingMode.NoSplitting)
      mode   <- schedulingMode(o)
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
    yield
      assertEquals(mode, SchedulingMode.NoSplitting)
      assert(!codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"unexpected ceiling violation: $codes")
