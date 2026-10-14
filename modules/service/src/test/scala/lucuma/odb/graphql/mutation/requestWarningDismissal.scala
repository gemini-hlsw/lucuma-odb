// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.GeminiCallForProposalsType.DemoScience
import lucuma.core.enums.Gender
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ProgramUserRole.*
import lucuma.core.model.PartnerLink
import lucuma.core.model.Program
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProgramUser
import lucuma.core.model.ProposalReference
import lucuma.core.model.Semester
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.odb.data.OdbError 

class requestWarningDismissal extends OdbSuite with query.ObservingModeSetupOperations:

  override val httpRequestHandler = invitationEmailRequestHandler

  val pi = TestUsers.Standard.pi(1, 101)
  val pi2 = TestUsers.Standard.pi(2, 102)
  val admin = TestUsers.Standard.admin(2, 103)
  val staff = TestUsers.Standard.staff(4, 104)

  val validUsers = List(pi, pi2, admin, staff)

  // senderEmail
  // recipientEmail
  // subject
  // textMessage
  // htmlMessage
  // originalTime
  // status
  // statusTime

  def requestQuery(pid: Program.Id, fields: List[String]): String =
    requestQueryWith(s"""programId: "$pid"""", fields)

  def requestQuery(ref: ProposalReference, fields: List[String]): String =
    requestQueryWith(s"""proposalReference: "${ref.label}"""", fields)

  def requestQuery(ref: ProgramReference, fields: List[String]): String =
    requestQueryWith(s"""programReference: "${ref.label}"""", fields)

  def requestQueryWith(programSelector: String, fields: List[String]): String =
    s"""
        mutation {
          requestWarningDismissal(
            input: {
              $programSelector
              text: "Sample text."
            }
          ) {
            ${fields.mkString(" ")}
          }
        }
      """
  
  def setEmailAddressAs(user: User, puid: ProgramUser.Id, email: String): IO[Unit] =
    updateProgramUserAs(user, puid, PartnerLink.HasNonPartner, email = Some(NonEmptyString.unsafeFrom(email)))

  def addProgramUserAs(user: User, pid: Program.Id, role: ProgramUserRole, email: String): IO[ProgramUser.Id] =
    addProgramUserAs(user, pid, role, PartnerLink.HasNonPartner, UserProfile.Empty.copy(email = Some(email)), EducationalStatus.Other, false, Gender.NotSpecified)

  // A submitted proposal whose PI has the default email address and with a
  // primary support contact.
  val submittedProgram: IO[(Program.Id, ProposalReference)] =
    for
      cid  <- createGeminiCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A"))
      pid  <- createProgramWithUsPi(pi)
      _    <- addDemoScienceProposal(pi, pid, cid)
      _    <- addDefinedObservationAs(pi, pid)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      ref  <- submitProposal(pi, pid)
    yield (pid, ref)

  def expectedSingleEmail(pid: Program.Id): Either[List[String], Json] =
    val subj = s"$pid: request to review warnings"
    Right(json"""
      {
        "requestWarningDismissal" : [
          {
            "senderEmail" : ${defaultPiEmail.value},
            "recipientEmail" : "steve@jobs.com",
            "subject": $subj,
            "textMessage" : "Sample text."
          }
        ]
      }
    """)

  test("can't send email if no pi address"):
    val ExpectedError = s"Current user ${pi.id} email address is missing or invalid."
    for
      pid  <- createProgramAs(pi)
      _    <-
        expectOdbError(
          user = pi,
          query = requestQuery(pid, List("senderEmail")),
          expected =
            case OdbError.EmailSendError(Some(ExpectedError)) => () // expected
        )
    yield ()

  test("can't send email if it's not my program"):
    val ExpectedError = s"No such program, or user ${pi2.id} is not authorized to perform this action."
    for
      pid  <- createProgramAs(pi)
      _    <-
        expectOdbError(
          user = pi2,
          query = requestQuery(pid, List("senderEmail")),
          expected =
            case OdbError.NotAuthorized(pi2, Some(ExpectedError)) => ()
        )
    yield ()

  test("can't send email if no support addresses"):
    val ExpectedError = s"Both SupportPrimary and SupportSecondary email addresses are missing or invalid."
    for
      pid  <- createProgramAs(pi)
      puid <- piProgramUserIdAs(pi, pid)
      _    <- setEmailAddressAs(pi, puid, "bob@dobbs.com")
      _    <-
        expectOdbError(
          user = pi,
          query = requestQuery(pid, List("senderEmail")),
          expected =
            case OdbError.EmailSendError(Some(ExpectedError)) => () // expected
        )
    yield ()

  List(SupportPrimary, SupportSecondary).foreach: role =>
    test(s"can send request with $role"):
      for
        pid  <- createProgramAs(pi)
        subj  = s"$pid: request to review warnings"
        puid <- piProgramUserIdAs(pi, pid)
        _    <- setEmailAddressAs(pi, puid, "bob@dobbs.com")
        _    <- addProgramUserAs(admin, pid, role, "steve@jobs.com")
        _    <-
          expect(
            user = pi,
            query = requestQuery(pid, List("senderEmail", "recipientEmail", "subject", "textMessage")),
            expected = Right(json"""
              {
                "requestWarningDismissal" : [
                  {
                    "senderEmail" : "bob@dobbs.com",
                    "recipientEmail" : "steve@jobs.com",
                    "subject": $subj,
                    "textMessage" : "Sample text."
                  }
                ]
              }
            """)
          )
      yield ()

  test(s"can send request with both"):
    for
      pid  <- createProgramAs(pi)
      subj  = s"$pid: request to review warnings"
      puid <- piProgramUserIdAs(pi, pid)
      _    <- setEmailAddressAs(pi, puid, "bob@dobbs.com")
      _    <- List(SupportPrimary, SupportSecondary).traverse(r => addProgramUserAs(admin, pid, r, s"$r@roles.com"))
      _    <-
        expect(
          user = pi,
          query = requestQuery(pid, List("senderEmail")),
          expected = Right(json"""
            {
              "requestWarningDismissal" : [
                {
                  "senderEmail" : "bob@dobbs.com"
                },
                {
                  "senderEmail" : "bob@dobbs.com"
                }
              ]
            }
          """)
        )
    yield ()
  test("can send request with a proposal reference"):
    for
      (pid, ref) <- submittedProgram
      _          <-
        expect(
          user = pi,
          query = requestQuery(ref, List("senderEmail", "recipientEmail", "subject", "textMessage")),
          expected = expectedSingleEmail(pid)
        )
    yield ()

  test("can send request with a program reference"):
    for
      (pid, _) <- submittedProgram
      ref      <- acceptProposal(staff, pid)
      _        <-
        expect(
          user = pi,
          query = requestQuery(ref.get, List("senderEmail", "recipientEmail", "subject", "textMessage")),
          expected = expectedSingleEmail(pid)
        )
    yield ()

  test("can't send email with a proposal reference to another user's program"):
    val ExpectedError = s"No such program, or user ${pi2.id} is not authorized to perform this action."
    for
      (_, ref) <- submittedProgram
      _        <-
        expectOdbError(
          user = pi2,
          query = requestQuery(ref, List("senderEmail")),
          expected =
            case OdbError.NotAuthorized(_, Some(ExpectedError)) => ()
        )
    yield ()

  test("can't send email with a program reference to another user's program"):
    val ExpectedError = s"No such program, or user ${pi2.id} is not authorized to perform this action."
    for
      (pid, _) <- submittedProgram
      ref      <- acceptProposal(staff, pid)
      _        <-
        expectOdbError(
          user = pi2,
          query = requestQuery(ref.get, List("senderEmail")),
          expected =
            case OdbError.NotAuthorized(_, Some(ExpectedError)) => ()
        )
    yield ()

  test("can't send email with an unknown proposal reference"):
    val ref           = ProposalReference.fromString.unsafeGet("G-2025A-9999")
    val ExpectedError = s"Proposal '${ref.label}' was not found."
    expectOdbError(
      user = pi,
      query = requestQuery(ref, List("senderEmail")),
      expected =
        case OdbError.InvalidArgument(Some(ExpectedError)) => ()
    )
