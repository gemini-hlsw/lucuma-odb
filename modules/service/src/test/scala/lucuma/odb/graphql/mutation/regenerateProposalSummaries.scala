// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.parser.decode
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.OdbError
import lucuma.odb.data.SummaryStyle
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.service.PdfSummaryJobPayload
import lucuma.odb.service.PdfSummaryJobService
import lucuma.odb.service.Services
import lucuma.odb.util.Codecs.*
import skunk.codec.all.*
import skunk.implicits.*

import scala.io.Source

class regenerateProposalSummaries extends OdbSuite
  with ObservingModeSetupOperations:

  val pi      = TestUsers.Standard.pi(1, 101)
  val pi2     = TestUsers.Standard.pi(2, 102)
  val ngoCa   = TestUsers.Standard.ngo(3, 103, Partner.CA)
  val staff   = TestUsers.Standard.staff(5, 105)
  val guest   = TestUsers.guest(6)
  val service = TestUsers.service(7)

  val validUsers = List(pi, pi2, ngoCa, staff, guest, service)

  override val httpRequestHandler = invitationEmailRequestHandler

  case class Job(
    id:       Long,
    partner:  Option[Partner],
    style:    SummaryStyle,
    state:    String,
    attempts: Int,
    retryAt:  Option[java.time.LocalDateTime],
    error:    Option[String]
  )

  def jobsFor(pid: Program.Id): IO[List[Job]] =
    withSession: s =>
      s.execute(
        sql"""
          SELECT c_summary_job_id, c_partner, c_style, c_state::text, c_attempts, c_retry_at, c_error
          FROM t_summary_job
          WHERE c_program_id = $program_id
          ORDER BY c_summary_job_id
        """.query(int8 *: partner.opt *: summary_style *: text *: int4 *: timestamp.opt *: text.opt)
      )(pid).map(_.map(Job.apply))

  def setJobColumns(pid: Program.Id, assignments: String): IO[Unit] =
    withSession: s =>
      s.execute(
        sql"""
          UPDATE t_summary_job
          SET #$assignments
          WHERE c_program_id = $program_id
        """.command
      )(pid).void

  // What the daemon does: take pending jobs one at a time until none is left.
  // Building the payload runs a GraphQL query, so a full mapping is needed.
  def nextAll(pid: Program.Id): IO[List[PdfSummaryJobService.Prepared]] =
    def loop(services: Services[IO])(using Services.ServiceAccess): IO[List[PdfSummaryJobService.Prepared]] =
      services.pdfSummaryJobService.next.flatMap:
        case None    => List.empty.pure[IO]
        case Some(p) => loop(services).map(p :: _)
    withServicesForObscalc(service)(loop).map(_.filter(_.job.programId === pid))

  // Then render, upload and finalize.
  def renderAll(pid: Program.Id): IO[Unit] =
    nextAll(pid).flatMap: prepared =>
      withServices(service): services =>
        prepared.traverse_(p => services.pdfSummaryJobService.finalize(p, fs2.Stream.empty))

  def failAll(pid: Program.Id, error: String, permanent: Boolean): IO[Unit] =
    nextAll(pid).flatMap: prepared =>
      withServices(service): services =>
        prepared.traverse_(p => services.transactionally(services.pdfSummaryJobService.fail(p.job, error, permanent)))

  // A submittable proposal with two partners and one ITC-capable observation.
  def setupProposal(splits: List[(Partner, Int)] = List((Partner.US, 70), (Partner.CA, 30))): IO[Program.Id] =
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addProposalPrerequisitesAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      _   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- if splits.isEmpty then IO.unit else addPartnerSplits(pi, pid, partnerSplits = splits)
      _   <- addCoisAs(pi, pid)
    yield pid

  def regenerate(user: User, pid: Program.Id): IO[Unit] =
    expect(
      user,
      s"""
        mutation {
          regenerateProposalSummaries(input: { programId: "$pid" }) {
            program { id }
          }
        }
      """,
      json"""{ "regenerateProposalSummaries": { "program": { "id": $pid } } }""".asRight
    )

  def regenerateExpectError(user: User, pid: Program.Id, msg: String): IO[Unit] =
    expect(
      user,
      s"""
        mutation {
          regenerateProposalSummaries(input: { programId: "$pid" }) {
            program { id }
          }
        }
      """,
      List(msg).asLeft
    )

  case class Summary(partner: Option[String], fileName: String, style: String)

  def summaries(user: User, pid: Program.Id): IO[List[Summary]] =
    query(
      user,
      s"""
        query {
          program(programId: "$pid") {
            attachments { attachmentType fileName proposalSummary { partner style } }
          }
        }
      """
    ).map: json =>
      json.hcursor.downField("program").downField("attachments").as[List[Json]].toOption.get
        .filter(_.hcursor.downField("attachmentType").as[String].toOption.contains("SUMMARY"))
        .map(a => Summary(
          a.hcursor.downField("proposalSummary").downField("partner").as[Option[String]].toOption.flatten,
          a.hcursor.downField("fileName").as[String].toOption.get,
          a.hcursor.downField("proposalSummary").downField("style").as[String].toOption.get
        ))

  lazy val fixture: Json =
    decode[Json](Source.fromResource("lucuma/odb/summary/payload-v1.json").mkString).toOption.get

  test("submission enqueues one pending job per partner"):
    for
      pid  <- setupProposal()
      _    <- submitProposal(pi, pid)
      jobs <- jobsFor(pid)
    yield
      assertEquals(jobs.map(j => (j.partner, j.style, j.state)), List(
        (Partner.CA.some, SummaryStyle.GeminiInvestigatorsAtEnd, "pending"),
        (Partner.US.some, SummaryStyle.NoirlabDarp,    "pending")
      ))

  test("a claimed job is rendering and its payload can be built"):
    for
      pid      <- setupProposal()
      _        <- submitProposal(pi, pid)
      prepared <- nextAll(pid)
      jobs     <- jobsFor(pid)
    yield
      assertEquals(jobs.map(_.state), List("rendering", "rendering"))
      assertEquals(prepared.map(_.fileName.value.endsWith("-summary-ca.pdf")).head, true)
      val payload = prepared.head.payload.hcursor
      assertEquals(payload.downField("schemaVersion").as[String], PdfSummaryJobPayload.SchemaVersion.asRight)
      assertEquals(
        payload.downField("program").downField("data").downField("program").downField("id").as[String],
        pid.toString.asRight
      )
      assertEquals(payload.downField("observations").as[List[Json]].map(_.size), 1.asRight)

  // The fixture omits the null fields of unused observing modes, so nested
  // objects are checked for containment; the envelope must match exactly.
  test("the payload has the shape of the json shared with pyexplore"):
    def keys(c: io.circe.ACursor): Set[String] = c.keys.toList.flatten.toSet

    def assertContains(payload: io.circe.ACursor, fix: io.circe.ACursor, path: String*): Unit =
      val p = path.foldLeft(payload)(_.downField(_))
      val f = path.foldLeft(fix)(_.downField(_))
      val missing = keys(f) -- keys(p)
      assert(missing.isEmpty, s"payload ${path.mkString("/")} lacks fixture keys $missing")

    for
      pid      <- setupProposal()
      _        <- submitProposal(pi, pid)
      prepared <- nextAll(pid)
    yield
      val payload = prepared.head.payload.hcursor
      val fix     = fixture.hcursor
      assertEquals(keys(payload), keys(fix))
      assertEquals(
        payload.downField("attachments").as[List[Json]].map(_.flatMap(_.hcursor.downField("fileName").as[String].toOption)),
        List("science.pdf", "team.pdf").asRight
      )
      assertEquals(keys(payload.downField("program")), keys(fix.downField("program")))
      assertEquals(
        keys(payload.downField("program").downField("data").downField("program")),
        keys(fix.downField("program").downField("data").downField("program"))
      )
      val payloadObs = payload.downField("observations").downArray
      val fixtureObs = fix.downField("observations").downArray
      assertEquals(keys(payloadObs), keys(fixtureObs))
      assertContains(payloadObs, fixtureObs, "observingMode")
      assertContains(payloadObs, fixtureObs, "observingMode", "gmosNorthLongSlit")
      assertContains(payloadObs, fixtureObs, "observingMode", "gmosNorthLongSlit", "exposureTimeMode")
      assertContains(payloadObs, fixtureObs, "targetEnvironment", "firstScienceTarget")
      assertContains(payloadObs, fixtureObs, "targetEnvironment", "firstScienceTarget", "sourceProfile")
      assertContains(payloadObs, fixtureObs, "targetEnvironment", "firstScienceTarget", "sidereal")
      assertContains(payloadObs, fixtureObs, "constraintSet")
      // The digest itself is only present once obscalc has run, so just its envelope is checked.
      assertContains(payloadObs, fixtureObs, "execution", "digest")

  // A draft may be regenerated too; the PI gets a preview of the summary.
  test("a proposal without partner splits gets a single default-style job"):
    for
      pid  <- setupProposal(splits = Nil)
      _    <- regenerate(pi, pid)
      jobs <- jobsFor(pid)
    yield assertEquals(jobs.map(j => (j.partner, j.style)), List((none, SummaryStyle.GeminiStandard)))

  test("regenerating while a job is waiting is a no-op"):
    for
      pid    <- setupProposal()
      _      <- submitProposal(pi, pid)
      before <- jobsFor(pid)
      _      <- regenerate(pi, pid)
      after  <- jobsFor(pid)
    yield
      assertEquals(after.map(_.id), before.map(_.id))
      assertEquals(after.map(_.state).toSet, Set("pending"))
      assertEquals(after.map(_.style), List(SummaryStyle.GeminiInvestigatorsAtEnd, SummaryStyle.NoirlabDarp))

  test("regenerating while a job is rendering enqueues a new one"):
    for
      pid    <- setupProposal()
      _      <- submitProposal(pi, pid)
      before <- nextAll(pid)
      _      <- regenerate(staff, pid)
      after  <- jobsFor(pid)
    yield
      assertEquals(after.size, before.size * 2)
      assertEquals(after.count(_.state === "pending"), before.size)

  test("only staff and the proposal's investigators may regenerate"):
    for
      pid <- setupProposal()
      _   <- submitProposal(pi, pid)
      _   <- regenerateExpectError(pi2, pid, OdbError.NotAuthorized(pi2.id).message)
      _   <- regenerateExpectError(ngoCa, pid, OdbError.NotAuthorized(ngoCa.id).message)
      _   <- regenerateExpectError(guest, pid, OdbError.NotAuthorized(guest.id).message)
      _   <- regenerate(staff, pid)
    yield ()

  test("a program without a proposal cannot be summarized"):
    for
      pid <- createProgramAs(pi)
      _   <- regenerateExpectError(pi, pid, s"Program $pid has no proposal to summarize.")
    yield ()

  test("a rendered job becomes a SUMMARY attachment for its partner, replacing the previous one, and is deleted"):
    for
      pid    <- setupProposal()
      _      <- submitProposal(pi, pid)
      _      <- renderAll(pid)
      left   <- jobsFor(pid)
      first  <- summaries(pi, pid)
      // A second round replaces the attachments rather than adding to them.
      _      <- regenerate(staff, pid)
      _      <- renderAll(pid)
      second <- summaries(pi, pid)
    yield
      assertEquals(left, Nil)
      assertEquals(first.map(s => (s.partner, s.fileName.endsWith(".pdf"), s.style)), List((Some("CA"), true, "GEMINI_INVESTIGATORS_AT_END"), (Some("US"), true, "NOIRLAB_DARP")))
      assertEquals(second.map(_.partner), List(Some("CA"), Some("US")))

  test("claiming honors the retry time"):
    for
      pid     <- setupProposal()
      _       <- submitProposal(pi, pid)
      _       <- setJobColumns(pid, "c_retry_at = now() + interval '1 hour'")
      none    <- nextAll(pid)
      _       <- setJobColumns(pid, "c_retry_at = NULL")
      claimed <- nextAll(pid)
      jobs    <- jobsFor(pid)
    yield
      assertEquals(none, Nil)
      assertEquals(claimed.size, 2)
      assertEquals(jobs.map(_.state).toSet, Set("rendering"))

  test("a transient failure is retried with backoff until the attempt cap"):
    for
      pid   <- setupProposal(splits = List((Partner.US, 100)))
      _     <- submitProposal(pi, pid)
      _     <- failAll(pid, "ITC unreachable", permanent = false)
      one   <- jobsFor(pid).map(_.head)
      _     <- setJobColumns(pid, "c_retry_at = now() - interval '1 second'")
      _     <- failAll(pid, "ITC unreachable", permanent = false)
      _     <- setJobColumns(pid, "c_retry_at = now() - interval '1 second'")
      _     <- failAll(pid, "ITC unreachable", permanent = false)
      three <- jobsFor(pid).map(_.head)
    yield
      assertEquals((one.state, one.attempts, one.error, one.retryAt.isDefined), ("pending", 1, "ITC unreachable".some, true))
      assertEquals((three.state, three.attempts, three.retryAt), ("failed", PdfSummaryJobService.MaxAttempts, none))

  test("a permanent failure is not retried"):
    for
      pid <- setupProposal(splits = List((Partner.US, 100)))
      _   <- submitProposal(pi, pid)
      _   <- failAll(pid, "Unknown style", permanent = true)
      job <- jobsFor(pid).map(_.head)
    yield assertEquals((job.state, job.attempts, job.error), ("failed", 1, "Unknown style".some))

  // Stale renders are swept when the next job is asked for.  With both jobs
  // stale and one out of attempts, the sweep re-pends the first (which is then
  // claimed) and fails the second.
  test("stale renders are re-pended, or failed when out of attempts"):
    for
      pid   <- setupProposal()
      _     <- submitProposal(pi, pid)
      jobs  <- jobsFor(pid)
      _     <- setJobColumns(pid, "c_state = 'rendering', c_started_at = now() - interval '2 hours'")
      _     <- withSession: s =>
                 s.execute(sql"UPDATE t_summary_job SET c_attempts = ${int4} WHERE c_summary_job_id = ${int8}".command)((PdfSummaryJobService.MaxAttempts, jobs(1).id))
      taken <- nextAll(pid)
      after <- jobsFor(pid)
    yield
      assertEquals(taken.map(_.job.id), List(jobs(0).id))
      assertEquals(after.map(_.state), List("rendering", "failed"))

