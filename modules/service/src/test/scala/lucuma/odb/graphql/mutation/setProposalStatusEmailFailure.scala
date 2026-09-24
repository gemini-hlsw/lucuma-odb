// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.effect.Resource
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import fs2.text.utf8
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.data.EmailAddress
import lucuma.core.enums.GeminiCallForProposalsType
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.odb.Config
import lucuma.refined.*
import org.http4s.Charset
import org.http4s.UrlForm
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits.*

/**
 * The submission emails are sent after the transaction commits, so the provider rejecting an
 * address can no longer un-submit a proposal that is already submitted, and can no longer be
 * swallowed either.  Each address is attempted independently, so one rejection does not silence
 * the rest.
 */
class setProposalStatusEmailFailure extends OdbSuite with query.ObservingModeSetupOperations {

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(4, 104)

  val validUsers = List(pi, staff)

  private def address(name: String): EmailAddress =
    EmailAddress.unsafeFrom(s"$name@gemini.edu")

  private val piAddress: EmailAddress =
    EmailAddress.unsafeFrom(defaultPiEmail.value)

  // The provider rejects these; everything else is accepted.
  private val rejected: Set[EmailAddress] =
    Set(piAddress, address("ca"))

  override val httpRequestHandler = req => {
    val dsl = Http4sDsl[IO]
    import dsl.*

    val recipient =
      req.body.through(utf8.decode).compile.toList
        .map(l => UrlForm.decodeString(Charset.`UTF-8`)(l.head).toOption.get.get("to").headOption.get)
        .map(EmailAddress.unsafeFrom)

    Resource.eval:
      recipient.flatMap: recip =>
        if rejected.contains(recip) then BadRequest("nope")
        else
          UUIDGen[IO].randomUUID
            .map(uuid => Json.obj("id" -> s"<$uuid>".asJson, "message" -> "Queued".asJson).toString)
            .flatMap(Ok(_))
  }

  override def emailConfig: Config.Email =
    Config.Email(
      apiKey            = "apiKey".refined,
      domain            = "gpp.com".refined,
      webhookSigningKey = "webhookKey".refined,
      invitationFrom    = EmailAddress.unsafeFrom("explore@gpp.com"),
      exploreUrl        = uri"https://explore.gemini.edu/",
      proposalEmails    = Config.ProposalEmails(
        demoScience        = address("demo-science"),
        directorsTime      = address("directors-time"),
        fastTurnaround     = address("fast-turnaround"),
        largeProgram       = address("large-program"),
        poorWeather        = address("poor-weather"),
        systemVerification = address("system-verification"),
        subaru             = address("subaru"),
        keck               = address("keck"),
        ar                 = address("ar"),
        br                 = address("br"),
        ca                 = address("ca"),
        cl                 = address("cl"),
        kr                 = address("kr"),
        uh                 = address("uh"),
        us                 = address("us")
      )
    )

  /** A submittable regular-semester proposal split between US and CA. */
  private def setup: IO[Program.Id] =
    for
      cid <- createGeminiCallForProposalsAs(staff, GeminiCallForProposalsType.RegularSemester)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addProposalPrerequisitesAs(pi, pid)
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 70), (Partner.CA, 30)))
      _   <- addCoisAs(pi, pid, List(Partner.US, Partner.CA))
      _   <- addDefinedObservationAs(pi, pid)
    yield pid

  private def submitExpectingEmailProblems(pid: Program.Id): IO[List[String]] =
    queryIor(pi, s"""
      mutation {
        setProposalStatus(input: { programId: "$pid", status: SUBMITTED }) {
          program { proposalStatus }
        }
      }
    """).map: ior =>
      // The mutation still returns data: the proposal *is* submitted.  The rejected addresses
      // ride along as problems, which is what a warning looks like over the wire.
      assert(ior.right.isDefined, s"Expected data alongside the errors, got $ior")
      assertEquals(
        ior.right.flatMap(_.hcursor.downFields("setProposalStatus", "program", "proposalStatus").as[String].toOption),
        "SUBMITTED".some
      )
      ior.left.foldMap(_.toList.map(_.message))

  private def proposalStatus(pid: Program.Id): IO[String] =
    query(pi, s"""query { program(programId: "$pid") { proposalStatus } }""")
      .map(_.hcursor.downFields("program", "proposalStatus").require[String])

  test("a rejected address does not un-submit the proposal"):
    for
      pid  <- setup
      msgs <- submitExpectingEmailProblems(pid)
      // The status must survive on its own, not merely in the mutation's response.
      _    <- assertIO(proposalStatus(pid), "SUBMITTED")
      _    <- IO(assert(msgs.nonEmpty, "The rejected addresses were swallowed and never reported."))
    yield ()

  test("every rejected address is reported, not just the first"):
    for
      pid  <- setup
      msgs <- submitExpectingEmailProblems(pid)
      // Both the PI address and the CA partner address are rejected.
      _    <- IO(assertEquals(msgs.count(_.contains("while attempting to send email")), 2, msgs.toString))
    yield ()

  test("a rejected address does not stop the others being emailed"):
    for
      pid <- setup
      _   <- submitExpectingEmailProblems(pid)
      // us@ is accepted and must still have been sent, even though the PI address was
      // rejected first and the CA address after it.
      _   <- assertIO(getEmailRecipients(pid), List(address("us")))
    yield ()
}
