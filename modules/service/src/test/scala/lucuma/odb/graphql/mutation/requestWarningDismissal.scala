// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.data.Ior
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.EducationalStatus
import lucuma.core.enums.EmailStatus
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
import lucuma.core.model.StandardRole
import lucuma.core.model.User
import lucuma.core.model.UserProfile
import lucuma.core.util.Gid
import lucuma.odb.data.OdbError
import org.http4s.Request
import org.http4s.Response
import org.http4s.Status
import org.http4s.UrlForm

class requestWarningDismissal extends OdbSuite with query.ObservingModeSetupOperations:

  // The address that dismissal requests are sent from, per the test email config.
  val SystemSender = "explore@gpp.com"

  // The form most recently sent to the mail provider for each recipient.
  val sentForms: Ref[IO, Map[String, UrlForm]] = Ref.unsafe(Map.empty)

  // Like `invitationEmailRequestHandler`, except that the mail provider rejects
  // any message addressed to a "bounce" recipient.
  override val httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] = req =>
    Resource.eval(req.as[UrlForm].flatTap(f => f.getFirst("to").traverse_(to => sentForms.update(_ + (to -> f))))).flatMap: form =>
      if form.getFirst("to").exists(_.startsWith("bounce")) then Resource.pure(Response[IO](Status.BadRequest))
      else invitationEmailRequestHandler(req)

  val pi = TestUsers.Standard.pi(1, 101)
  val pi2 = TestUsers.Standard.pi(2, 102)
  val admin = TestUsers.Standard.admin(3, 103)
  val staff = TestUsers.Standard.staff(4, 104)

  // A PI whose only email address comes from their ORCID profile.
  val orcidPi = TestUsers.Standard(5, StandardRole.Pi(Gid[StandardRole.Id].fromLong.getOption(105).get), email = Some("orcid@pi.com"))

  val validUsers = List(pi, pi2, admin, staff, orcidPi)

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

  val Justification = "These warnings are expected for this target."

  // The plain text body we expect for a request from `user` about the program
  // identified by `label` (its reference, or its id if it has no reference).
  def expectedText(user: User, label: String): String =
    s"${user.displayName} has requested review for dismissal of the warnings in program $label (https://explore.gemini.edu/$label).\n\n$Justification"

  def requestQueryWith(programSelector: String, fields: List[String], justification: String = Justification): String =
    s"""
        mutation {
          requestWarningDismissal(
            input: {
              $programSelector
              justification: ${Json.fromString(justification).noSpaces}
            }
          ) {
            ${fields.mkString(" ")}
          }
        }
      """
  
  def programEmailsQuery(pid: Program.Id, fields: List[String]): String =
    s"""
        query {
          program(programId: "$pid") {
            emails { ${fields.mkString(" ")} }
          }
        }
      """

  def bounceError(role: ProgramUserRole, address: String): OdbError =
    OdbError.EmailSendError(s"Could not send the request to $role $address: Unexpected status '400 Bad Request' while attempting to send email.".some)

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

  def expectedSingleEmail(label: String): Either[List[String], Json] =
    val subj = s"$label: request to review warnings"
    val text = expectedText(pi, label)
    Right(json"""
      {
        "requestWarningDismissal" : [
          {
            "senderEmail" : $SystemSender,
            "replyToEmail" : ${defaultPiEmail.value},
            "recipientEmail" : "steve@jobs.com",
            "subject": $subj,
            "textMessage" : $text
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
            case OdbError.NotAuthorized(uid, Some(ExpectedError)) if uid === pi2.id => ()
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
            query = requestQuery(pid, List("senderEmail", "replyToEmail", "recipientEmail", "subject", "textMessage")),
            expected = Right(json"""
              {
                "requestWarningDismissal" : [
                  {
                    "senderEmail" : $SystemSender,
                    "replyToEmail" : "bob@dobbs.com",
                    "recipientEmail" : "steve@jobs.com",
                    "subject": $subj,
                    "textMessage" : ${expectedText(pi, pid.toString)}
                  }
                ]
              }
            """)
          )
      yield ()

  test(s"can send request with both"):
    for
      pid  <- createProgramAs(pi)
      puid <- piProgramUserIdAs(pi, pid)
      _    <- setEmailAddressAs(pi, puid, "bob@dobbs.com")
      _    <- List(SupportSecondary, SupportPrimary).traverse(r => addProgramUserAs(admin, pid, r, s"$r@roles.com"))
      _    <-
        expect(
          user = pi,
          query = requestQuery(pid, List("replyToEmail", "recipientEmail")),
          expected = Right(json"""
            {
              "requestWarningDismissal" : [
                {
                  "replyToEmail" : "bob@dobbs.com",
                  "recipientEmail" : "SupportPrimary@roles.com"
                },
                {
                  "replyToEmail" : "bob@dobbs.com",
                  "recipientEmail" : "SupportSecondary@roles.com"
                }
              ]
            }
          """)
        )
    yield ()

  test("can send request with a proposal reference"):
    for
      (_, ref) <- submittedProgram
      _        <-
        expect(
          user = pi,
          query = requestQuery(ref, List("senderEmail", "replyToEmail", "recipientEmail", "subject", "textMessage")),
          expected = expectedSingleEmail(ref.label) // no program reference until accepted
        )
    yield ()

  test("can send request with a program reference"):
    for
      (pid, _) <- submittedProgram
      ref      <- acceptProposal(staff, pid)
      _        <-
        expect(
          user = pi,
          query = requestQuery(ref.get, List("senderEmail", "replyToEmail", "recipientEmail", "subject", "textMessage")),
          expected = expectedSingleEmail(ref.get.label)
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
            case OdbError.NotAuthorized(uid, Some(ExpectedError)) if uid === pi2.id => ()
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
            case OdbError.NotAuthorized(uid, Some(ExpectedError)) if uid === pi2.id => ()
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

  test("can send request with only an ORCID email address"):
    for
      pid  <- createProgramAs(orcidPi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      _    <-
        expect(
          user = orcidPi,
          query = requestQuery(pid, List("replyToEmail", "recipientEmail")),
          expected = Right(json"""
            {
              "requestWarningDismissal" : [
                {
                  "replyToEmail" : "orcid@pi.com",
                  "recipientEmail" : "steve@jobs.com"
                }
              ]
            }
          """)
        )
    yield ()

  test("program emails include the request"):
    for
      pid  <- createProgramWithUsPi(pi)
      subj  = s"$pid: request to review warnings"
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      _    <- query(pi, requestQuery(pid, List("senderEmail")))
      _    <-
        expect(
          user = pi,
          query = programEmailsQuery(pid, List("senderEmail", "replyToEmail", "recipientEmail", "subject", "textMessage", "status")),
          expected = Right(json"""
            {
              "program" : {
                "emails" : [
                  {
                    "senderEmail" : $SystemSender,
                    "replyToEmail" : ${defaultPiEmail.value},
                    "recipientEmail" : "steve@jobs.com",
                    "subject" : $subj,
                    "textMessage" : ${expectedText(pi, pid.toString)},
                    "status" : ${EmailStatus.Queued}
                  }
                ]
              }
            }
          """)
        )
    yield ()

  test("another user can't see the program emails"):
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      _    <- query(pi, requestQuery(pid, List("senderEmail")))
      _    <-
        expect(
          user = pi2,
          query = programEmailsQuery(pid, List("senderEmail")),
          expected = Right(json"""{ "program" : null }""")
        )
    yield ()

  test("returns the sent email and an error when one recipient fails"):
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      _    <- addProgramUserAs(admin, pid, SupportSecondary, "bounce@example.com")
      _    <-
        expectIor(
          user = pi,
          query = requestQuery(pid, List("recipientEmail")),
          expected = Ior.both(
            List(bounceError(SupportSecondary, "bounce@example.com").message),
            json"""
              {
                "requestWarningDismissal" : [
                  {
                    "recipientEmail" : "steve@jobs.com"
                  }
                ]
              }
            """
          )
        )
      _    <-
        expect(
          user = pi,
          query = programEmailsQuery(pid, List("recipientEmail")),
          expected = Right(json"""
            {
              "program" : {
                "emails" : [
                  {
                    "recipientEmail" : "steve@jobs.com"
                  }
                ]
              }
            }
          """)
        )
    yield ()

  test("reports an error for each recipient when all fail"):
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "bounce1@example.com")
      _    <- addProgramUserAs(admin, pid, SupportSecondary, "bounce2@example.com")
      _    <-
        expectOdbErrors(
          user = pi,
          query = requestQuery(pid, List("recipientEmail")),
          expected = Set(
            bounceError(SupportPrimary, "bounce1@example.com"),
            bounceError(SupportSecondary, "bounce2@example.com")
          )
        )
      _    <-
        expect(
          user = pi,
          query = programEmailsQuery(pid, List("recipientEmail")),
          expected = Right(json"""{ "program" : { "emails" : [] } }""")
        )
    yield ()

  test("sends from the system address with the PI as reply-to"):
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "reply-to-check@jobs.com")
      _    <- query(pi, requestQuery(pid, List("senderEmail")))
      form <- sentForms.get.map(_.get("reply-to-check@jobs.com"))
    yield
      assertEquals(form.flatMap(_.getFirst("from")), Some(SystemSender))
      assertEquals(form.flatMap(_.getFirst("h:Reply-To")), Some(defaultPiEmail.value))

  test("reports every invalid input field"):
    expectIor(
      user = pi,
      query = requestQueryWith("""programId: "not-a-pid", proposalReference: "not-a-ref"""", List("senderEmail")),
      expected = Ior.left(List(
        "Argument 'input.programId' is invalid: 'not-a-pid' is not a valid program id",
        "Argument 'input.proposalReference' is invalid: 'not-a-ref' cannot be parsed as a valid ProposalReference."
      ))
    )

  test("html message links to the program and escapes the justification"):
    val justification = "Line one <b>is</b> fine.\nLine two & more."
    for
      (pid, _) <- submittedProgram
      ref      <- acceptProposal(staff, pid).map(_.get)
      label     = ref.label
      _        <-
        expect(
          user = pi,
          query = requestQueryWith(s"""programReference: "$label"""", List("subject", "textMessage", "htmlMessage"), justification),
          expected = Right(json"""
            {
              "requestWarningDismissal" : [
                {
                  "subject" : ${s"$label: request to review warnings"},
                  "textMessage" : ${s"${pi.displayName} has requested review for dismissal of the warnings in program $label (https://explore.gemini.edu/$label).\n\n$justification"},
                  "htmlMessage" : ${s"""${pi.displayName} has requested review for dismissal of the warnings in program <a href="https://explore.gemini.edu/$label">$label</a>.<br/>\n<br/>\nLine one &lt;b&gt;is&lt;/b&gt; fine.<br/>\nLine two &amp; more."""}
                }
              ]
            }
          """)
        )
    yield ()

  test("staff who are not users of the program can't send a request"):
    val ExpectedError = "Only users of the program may request that warnings be dismissed."
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      _    <-
        expectOdbError(
          user = staff,
          query = requestQuery(pid, List("senderEmail")),
          expected =
            case OdbError.NotAuthorized(uid, Some(ExpectedError)) if uid === staff.id => ()
        )
    yield ()

  test("sends one email to a support address shared by both roles"):
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "desk@jobs.com")
      _    <- addProgramUserAs(admin, pid, SupportSecondary, "desk@jobs.com")
      _    <-
        expect(
          user = pi,
          query = requestQuery(pid, List("recipientEmail")),
          expected = Right(json"""
            {
              "requestWarningDismissal" : [
                {
                  "recipientEmail" : "desk@jobs.com"
                }
              ]
            }
          """)
        )
    yield ()

  test("program emails are visible to the PI and staff but not to other program users"):
    val oneEmail = json"""{ "program" : { "emails" : [ { "recipientEmail" : "steve@jobs.com" } ] } }"""
    for
      pid  <- createProgramWithUsPi(pi)
      _    <- addProgramUserAs(admin, pid, SupportPrimary, "steve@jobs.com")
      coi  <- addProgramUserAs(pi, pid, Coi, "coi@jobs.com")
      _    <- linkUserAs(staff, coi, pi2.id)
      _    <- query(pi, requestQuery(pid, List("senderEmail")))
      _    <- expect(pi, programEmailsQuery(pid, List("recipientEmail")), Right(oneEmail))
      _    <- expect(staff, programEmailsQuery(pid, List("recipientEmail")), Right(oneEmail))
      _    <- expect(pi2, programEmailsQuery(pid, List("recipientEmail")), Right(json"""{ "program" : { "emails" : [] } }"""))
    yield ()
