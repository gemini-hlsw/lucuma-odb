// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import io.circe.syntax.*
import lucuma.core.enums.GeminiCallForProposalsType.RegularSemester
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

/**
 * The Target-of-Opportunity ceiling: an unset proposal-level activation is
 * derived as the maximum among the program's observations, frozen on
 * acceptance, and thereafter enforced against each observation.
 */
class tooActivationCeiling extends OdbSuite with ObservingModeSetupOperations:

  val pi:      User = TestUsers.Standard.pi(1, 101)
  val staff:   User = TestUsers.Standard.staff(3, 103)
  val service      = TestUsers.service(4)

  override val validUsers: List[User] = List(pi, staff)

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
      _ <- computeItcResultAs(pi, o)
      _ <- setTooActivation(o, activation)
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
      _      <- setTooActivation(o2, "INTERRUPTING")
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
      _      <- setTooActivation(o2, "INTERRUPTING")
      after  <- proposalCeiling(p)
    yield
      assertEquals(before, "RAPID")
      assertEquals(after,  "RAPID") // frozen: the new observation does not raise its own ceiling

  test("an observation exceeding the frozen ceiling is flagged and cannot become ready"):
    for
      (p, o) <- setup("RAPID")
      _      <- acceptProposal(staff, p)
      _      <- setTooActivation(o, "INTERRUPTING")
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
      _      <- setTooActivation(o, "STANDARD")
      _      <- runObscalcUpdateAs(service, p, o)
      codes  <- validationCodes(o)
    yield assert(!codes.contains("TOO_ACTIVATION_UNAPPROVED"), s"unexpected ceiling violation: $codes")
