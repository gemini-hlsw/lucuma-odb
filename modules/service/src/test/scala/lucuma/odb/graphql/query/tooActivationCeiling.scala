// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ExchangeObservingModeType
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.TooActivation
import lucuma.core.enums.TooActivation.Interrupting
import lucuma.core.enums.TooActivation.Rapid
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.string.*
import lucuma.odb.data.OdbError

/**
 * The Target-of-Opportunity activation an observation may reach: its program's
 * ceiling.
 *
 * A program with no ceiling has no restriction.  Acceptance gives a program one
 * if it has none -- `NONE` for classical, poor weather and Keck proposals, and
 * otherwise the highest activation among its observations -- and staff may set
 * or clear it on any program as a program property.
 * A set ceiling is enforced whatever kind of program it is on.  A proposal type
 * that ordinarily has no ToOs is only warned about until a ceiling is set.
 */
class tooActivationCeiling extends OdbSuite with TooTriggerSetupOperations:

  val pi:      User = TestUsers.Standard.pi(1, 101)
  val staff:   User = TestUsers.Standard.staff(3, 103)
  val service      = TestUsers.service(4)

  override val validUsers: List[User] = List(pi, staff)

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

  private def observationActivation(oid: Observation.Id): IO[TooActivation] =
    query(
      pi,
      s"""
        query {
          observation(observationId: ${oid.asJson}) {
            schedulingConstraints { tooActivation }
          }
        }
      """
    ).map(_.hcursor.downFields("observation", "schedulingConstraints", "tooActivation").require[TooActivation])

  private def ceiling(pid: Program.Id): IO[Option[TooActivation]] =
    query(pi, s"""query { program(programId: ${pid.asJson}) { tooActivationCeiling } }""")
      .map(_.hcursor.downFields("program", "tooActivationCeiling").require[Option[TooActivation]])

  private def summaries(pid: Program.Id): IO[(TooActivation, SchedulingMode)] =
    query(pi, s"""query { program(programId: ${pid.asJson}) { maxTooActivation maxSchedulingMode } }""")
      .map: js =>
        val c = js.hcursor.downField("program")
        (c.downField("maxTooActivation").require[TooActivation], c.downField("maxSchedulingMode").require[SchedulingMode])

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

  private def flagged(pid: Program.Id, oid: Observation.Id): IO[Boolean] =
    runObscalcUpdateAs(service, pid, oid) *> validationCodes(oid).map(_.contains("TOO_ACTIVATION_UNAPPROVED"))

  private def warned(pid: Program.Id, oid: Observation.Id): IO[Boolean] =
    runObscalcUpdateAs(service, pid, oid) *> validationCodes(oid).map(_.contains("TOO_ACTIVATION_UNEXPECTED"))

  private val Classical: String = "classical: { minPercentTime: 50 }"

  /**
   * A program with a proposal (queue unless `proposalType` says otherwise) and one
   * observation declaring `activation`.
   */
  private def setup(activation: TooActivation, proposalType: Option[String] = None): IO[(Program.Id, Observation.Id)] =
    for
      _ <- createUsers(pi, staff)
      c <- createGeminiCallForProposalsAs(staff, RegularSemester)
      p <- createProgramWithNonPartnerPi(pi, "ToO ceiling")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- setTooActivationAs(pi, o, activation)
      _ <- computeItcResultAs(pi, o)
      _ <- addProposal(pi, p, c.some, proposalType)
      _ <- addPartnerSplits(pi, p, proposalType.fold("queue")(_.takeWhile(_ != ':')))
      _ <- addCoisAs(pi, p)
    yield (p, o)

  /** A program with no proposal and one observation declaring `activation`. */
  private def setupWithoutProposal(activation: TooActivation): IO[(Program.Id, Observation.Id)] =
    for
      _ <- createUsers(pi, staff)
      p <- createProgramAs(pi, "No proposal")
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- setTooActivationAs(pi, o, activation)
    yield (p, o)

  private def updateProgramQuery(pid: Program.Id, set: String): String =
    s"""
      mutation {
        updatePrograms(input: {
          SET: { $set }
          WHERE: { id: { EQ: ${pid.asJson} } }
        }) {
          programs { id }
        }
      }
    """

  private def setCeilingQuery(pid: Program.Id, ceiling: Option[TooActivation]): String =
    updateProgramQuery(pid, s"tooActivationCeiling: ${ceiling.fold("null")(_.tag.toScreamingSnakeCase)}")

  private def createProgramWithCeilingQuery(ceiling: TooActivation): String =
    s"""
      mutation {
        createProgram(input: { SET: { tooActivationCeiling: ${ceiling.tag.toScreamingSnakeCase} } }) {
          program { tooActivationCeiling }
        }
      }
    """

  private def setCeiling(pid: Program.Id, ceiling: Option[TooActivation]): IO[Unit] =
    query(staff, setCeilingQuery(pid, ceiling)).void

  // -- Summaries ----------------------------------------------------------------

  test("the program summarizes the highest activation and mode among its observations"):
    for
      (p, _) <- setup(Rapid)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _      <- setSchedulingModeAs(pi, o2, SchedulingMode.NoSplitting)
      sums   <- summaries(p)
    yield assertEquals(sums, (Rapid, SchedulingMode.Uninterruptible))

  test("a program with no observations summarizes to the lowest levels"):
    for
      _    <- createUsers(pi)
      p    <- createProgramAs(pi, "Empty")
      sums <- summaries(p)
    yield assertEquals(sums, (TooActivation.None, SchedulingMode.Unconstrained))

  // A summary of what the observations ask for, whatever the proposal type allows.
  test("the activation summary is not capped by the proposal type"):
    for
      (p, _) <- setup(Rapid, Classical.some)
      sums   <- summaries(p)
    yield assertEquals(sums._1, Rapid)

  // -- The default at acceptance -------------------------------------------------

  test("a program has no ceiling until acceptance"):
    for
      (p, o) <- setup(Interrupting)
      _      <- setProposalStatus(staff, p, "SUBMITTED")
      too    <- ceiling(p)
      flag   <- flagged(p, o)
    yield
      assertEquals(too, none)
      assert(!flag)

  test("acceptance sets the ceiling to the highest activation among the observations"):
    for
      (p, _) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      too    <- ceiling(p)
    yield assertEquals(too, Rapid.some)

  test("acceptance sets a classical proposal's ceiling to NONE"):
    for
      (p, o) <- setup(Rapid, Classical.some)
      _      <- acceptProposal(staff, p)
      too    <- ceiling(p)
      flag   <- flagged(p, o)
    yield
      assertEquals(too, TooActivation.None.some)
      assert(flag, "expected the unapproved activation to be flagged")

  // Only a missing ceiling is filled in; one staff chose, say during review, stays.
  test("acceptance keeps a ceiling staff already set"):
    for
      (p, _) <- setup(Rapid)
      _      <- setCeiling(p, Interrupting.some)
      _      <- acceptProposal(staff, p)
      too    <- ceiling(p)
    yield assertEquals(too, Interrupting.some)

  // -- Enforcement ----------------------------------------------------------------

  test("an activation within the ceiling is not flagged"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      flag   <- flagged(p, o)
    yield assert(!flag)

  test("an activation above the ceiling is flagged and cannot become ready"):
    for
      (p, o) <- setup(TooActivation.None)
      _      <- acceptProposal(staff, p)
      _      <- setTooActivationAs(pi, o, Rapid)
      flag   <- flagged(p, o)
      state  <- workflowState(o)
    yield
      assert(flag, "expected the unapproved activation to be flagged")
      assertEquals(state, "UNAPPROVED")

  test("raising the ceiling clears the flag"):
    for
      (p, o) <- setup(TooActivation.None)
      _      <- acceptProposal(staff, p)
      _      <- setTooActivationAs(pi, o, Rapid)
      before <- flagged(p, o)
      _      <- setCeiling(p, Rapid.some)
      after  <- flagged(p, o)
    yield
      assert(before)
      assert(!after)

  test("lowering the ceiling flags the observation"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      _      <- setCeiling(p, TooActivation.None.some)
      flag   <- flagged(p, o)
    yield assert(flag, "expected the unapproved activation to be flagged")

  test("clearing the ceiling lifts the restriction"):
    for
      (p, o) <- setup(TooActivation.None)
      _      <- acceptProposal(staff, p)
      _      <- setTooActivationAs(pi, o, Interrupting)
      before <- flagged(p, o)
      _      <- setCeiling(p, none)
      after  <- flagged(p, o)
      too    <- ceiling(p)
    yield
      assert(before)
      assert(!after)
      assertEquals(too, none)

  test("a PI may lower the activation to recover the observation"):
    for
      (p, o)  <- setup(TooActivation.None)
      _       <- acceptProposal(staff, p)
      _       <- setTooActivationAs(pi, o, Rapid)
      before  <- flagged(p, o)
      _       <- setTooActivationAs(pi, o, TooActivation.None)
      after   <- flagged(p, o)
    yield
      assert(before)
      assert(!after)

  test("a program without a proposal has no ceiling and no restriction"):
    for
      (p, o) <- setupWithoutProposal(Interrupting)
      too    <- ceiling(p)
      flag   <- flagged(p, o)
    yield
      assertEquals(too, none)
      assert(!flag)

  test("a ceiling set on a program without a proposal is enforced"):
    for
      (p, o) <- setupWithoutProposal(Rapid)
      _      <- setCeiling(p, TooActivation.None.some)
      flag   <- flagged(p, o)
    yield assert(flag, "expected the unapproved activation to be flagged")

  // -- Setting the ceiling ----------------------------------------------------------

  test("a PI may not set the ceiling"):
    for
      (p, _) <- setup(Rapid)
      _      <- expectOdbError(
                  user     = pi,
                  query    = setCeilingQuery(p, Interrupting.some),
                  expected = { case OdbError.NotAuthorized(_, _) => () }
                )
    yield ()

  test("editing other program properties leaves the ceiling alone"):
    for
      (p, _) <- setup(Rapid)
      _      <- setCeiling(p, Interrupting.some)
      _      <- query(staff, updateProgramQuery(p, "name: \"Renamed\""))
      too    <- ceiling(p)
    yield assertEquals(too, Interrupting.some)

  test("staff may create a program with a ceiling"):
    for
      _ <- createUsers(staff)
      _ <- expect(
             staff,
             createProgramWithCeilingQuery(Rapid),
             json"""{ "createProgram": { "program": { "tooActivationCeiling": "RAPID" } } }""".asRight
           )
    yield ()

  test("a PI may not create a program with a ceiling"):
    for
      _ <- createUsers(pi)
      _ <- expectOdbError(
             user     = pi,
             query    = createProgramWithCeilingQuery(Rapid),
             expected = { case OdbError.NotAuthorized(_, _) => () }
           )
    yield ()

  test("staff may grant a ToO ceiling to a classical program"):
    for
      (p, o) <- setup(Rapid, Classical.some)
      _      <- acceptProposal(staff, p)
      _      <- setCeiling(p, Rapid.some)
      flag   <- flagged(p, o)
    yield assert(!flag)

  // -- Raising the activation ------------------------------------------------------

  // Only a Ready observation is refused a raise (see tooTriggerActivation); a
  // Defined one simply goes Unapproved until staff raise the ceiling.
  test("a Defined observation may be raised above the ceiling"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      _      <- setTooActivationAs(pi, o, Interrupting)
      act    <- observationActivation(o)
      flag   <- flagged(p, o)
    yield
      assertEquals(act, Interrupting)
      assert(flag, "expected the unapproved activation to be flagged")

  test("the mode is free again once the activation is lowered"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      // Every ToO is UNINTERRUPTIBLE, so the mode is only free once the
      // observation stops being one.
      _      <- setTooActivationAs(pi, o, TooActivation.None)
      _      <- setSchedulingModeAs(pi, o, SchedulingMode.Unconstrained)
      mode   <- schedulingMode(o)
    yield assertEquals(mode, SchedulingMode.Unconstrained)

  test("a non-ToO observation may take any mode"):
    for
      (p, o) <- setup(Rapid)
      _      <- acceptProposal(staff, p)
      t      <- createTargetWithProfileAs(pi, p)
      o2     <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      // Born NONE, like every new observation, which is what a monitoring or
      // follow-up observation alongside a ToO wants.  The mode is the other axis:
      // an ordinary observation that must not be disturbed is ordinary science.
      _      <- setSchedulingModeAs(pi, o2, SchedulingMode.Uninterruptible)
      mode   <- schedulingMode(o2)
      act    <- observationActivation(o2)
    yield
      assertEquals(mode, SchedulingMode.Uninterruptible)
      assertEquals(act, TooActivation.None)


  // -- Proposal types that ordinarily have no ToOs --------------------------------

  // Staff may grant one, so the activation is not refused; the PI is warned
  // instead, before acceptance limits it to NONE.
  test("a ToO in a classical proposal is warned about, not refused"):
    for
      (p, o) <- setup(Rapid, Classical.some)
      act    <- observationActivation(o)
      warn   <- warned(p, o)
      flag   <- flagged(p, o)
    yield
      assertEquals(act, Rapid)
      assert(warn, "expected the unexpected activation to be warned about")
      assert(!flag)

  test("the warning gives way once a ceiling is set"):
    for
      (p, o) <- setup(Rapid, Classical.some)
      _      <- setCeiling(p, Rapid.some)
      warn   <- warned(p, o)
    yield assert(!warn)

  test("a ToO in an ordinary queue proposal draws no warning"):
    for
      (p, o) <- setup(Rapid)
      warn   <- warned(p, o)
    yield assert(!warn)

  // Changing the proposal type does not touch the observations, so the warning is
  // what catches a ToO left behind in a type that ordinarily has none.
  test("a ToO left behind by a change to classical is warned about"):
    for
      (p, o) <- setup(Rapid)
      _      <- query(
                  pi,
                  s"""
                    mutation {
                      updateProposal(input: {
                        programId: "$p"
                        SET: { gemini: { classical: { } } }
                      }) { proposal { category } }
                    }
                  """
                )
      warn   <- warned(p, o)
    yield assert(warn, "expected the unexpected activation to be warned about")

  // -- Exchange proposals --------------------------------------------------------
  //
  // Subaru adopted our nomenclature, so an rToO is RAPID.  What Subaru calls an
  // sToO -- observed whenever convenient once its event arrives -- is NONE here,
  // exactly as it is at Gemini.  A Subaru program's ceiling works like any other.
  // Keck proposals ordinarily have no ToOs: nobody has asked for them there.

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

  /** An exchange program with one exchange observation declaring `activation`. */
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
      _ <- setTooActivationAs(pi, o, activation)
      _ <- addCoisAs(pi, p)
    yield (p, o)

  private def subaruSetup(activation: TooActivation): IO[(Program.Id, Observation.Id)] =
    exchangeSetup(ExchangeObservingModeType.ExchangeSubaru, activation)

  test("a Subaru exchange rapid ToO is neither flagged nor warned about"):
    for
      (p, o) <- subaruSetup(Rapid)
      flag   <- flagged(p, o)
      warn   <- warned(p, o)
      state  <- workflowState(o)
    yield
      assert(!flag)
      assert(!warn)
      assertEquals(state, "DEFINED")

  test("a Subaru exchange ToO is held to the ceiling like any other"):
    for
      (p, o)  <- subaruSetup(Rapid)
      _       <- acceptProposal(staff, p)
      within  <- flagged(p, o)
      _       <- setTooActivationAs(pi, o, Interrupting)
      above   <- flagged(p, o)
    yield
      assert(!within)
      assert(above, "expected the unapproved activation to be flagged")

  test("a Keck exchange ToO is warned about, not refused"):
    for
      (p, o) <- exchangeSetup(ExchangeObservingModeType.ExchangeKeck, Rapid)
      warn   <- warned(p, o)
    yield assert(warn, "expected the unexpected activation to be warned about")

  // Defining one is supported, triggering it is not.  Requesting a trigger is
  // what setting an observation READY means, and exchange observations have no
  // such lifecycle -- they execute at Subaru.
  test("a Subaru exchange ToO is still never offered READY"):
    for
      (p, o) <- subaruSetup(Rapid)
      _      <- runObscalcUpdateAs(service, p, o)
      ts     <- validTransitions(o)
    yield assertEquals(ts, List("INACTIVE"))
