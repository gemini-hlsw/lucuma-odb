// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import lucuma.odb.graphql.query.ObservingModeSetupOperations
import lucuma.odb.util.Codecs.*
import org.typelevel.otel4s.trace.Tracer
import skunk.codec.all.*
import skunk.implicits.*

import scala.concurrent.duration.*

class PdfSummaryJobDaemonSuite extends OdbSuite with ObservingModeSetupOperations {

  val pi      = TestUsers.Standard.pi(1, 101)
  val staff   = TestUsers.Standard.staff(5, 105)
  val service = TestUsers.service(7)

  val validUsers = List(pi, staff, service)

  override val httpRequestHandler = invitationEmailRequestHandler

  import Tracer.Implicits.noop

  def jobsFor(pid: Program.Id): IO[List[(String, Int, Option[String])]] =
    withSession: s =>
      s.execute(
        sql"""
          SELECT c_state::text, c_attempts, c_error
          FROM t_summary_job
          WHERE c_program_id = $program_id
          ORDER BY c_summary_job_id
        """.query(text *: int4 *: text.opt)
      )(pid)

  def summaries(pid: Program.Id): IO[List[(Option[String], Long)]] =
    query(
      pi,
      s"""
        query {
          program(programId: "$pid") {
            attachments { attachmentType fileSize proposalSummary { partner } }
          }
        }
      """
    ).map: json =>
      json.hcursor.downField("program").downField("attachments").as[List[Json]].toOption.get
        .filter(_.hcursor.downField("attachmentType").as[String].toOption.contains("SUMMARY"))
        .map(a => (
          a.hcursor.downField("proposalSummary").downField("partner").as[Option[String]].toOption.flatten,
          a.hcursor.downField("fileSize").as[Long].toOption.get
        ))

  def setupProposal: IO[Program.Id] =
    for
      cid <- createGeminiCallForProposalsAs(staff)
      pid <- createProgramWithNonPartnerPi(pi)
      _   <- addProposal(pi, pid, cid.some)
      _   <- addProposalPrerequisitesAs(pi, pid)
      tid <- createTargetWithProfileAs(pi, pid)
      _   <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- addPartnerSplits(pi, pid, partnerSplits = List((Partner.US, 70), (Partner.CA, 30)))
      _   <- addCoisAs(pi, pid)
    yield pid

  // The daemon as Main wires it, on this suite's database, with the given renderer.
  def daemon(renderer: PdfRenderer[IO])(body: IO[Unit]): IO[Unit] =
    withServicesResourceForObscalc(service): services =>
      PdfSummaryJobDaemon.run(200.millis, session, services, renderer).use(_ => body)

  // Polls until every job of the program is gone (rendered) or failed.
  def awaitSettled(pid: Program.Id): IO[List[(String, Int, Option[String])]] =
    def go(left: Int): IO[List[(String, Int, Option[String])]] =
      jobsFor(pid).flatMap: jobs =>
        if jobs.forall(_._1 == "failed") then jobs.pure[IO]
        else if left <= 0 then IO.raiseError(new RuntimeException(s"Summary jobs did not settle: $jobs"))
        else IO.sleep(250.millis) *> go(left - 1)
    go(120)

  test("a submitted proposal is rendered into one SUMMARY attachment per partner") {
    val pdf = "%PDF-1.4 stub"
    daemon(PdfRenderer.constant[IO](pdf)):
      for
        pid  <- setupProposal
        _    <- submitProposal(pi, pid)
        jobs <- awaitSettled(pid)
        atts <- summaries(pid)
      yield
        assertEquals(jobs, Nil)
        assertEquals(atts.map(_._1), List(Some("CA"), Some("US")))
  }

  test("a permanent render failure is recorded on the job") {
    daemon(PdfRenderer.failing[IO](PdfRenderer.Error("Renderer exited with code 2: Unknown style", permanent = true))):
      for
        pid  <- setupProposal
        _    <- submitProposal(pi, pid)
        jobs <- awaitSettled(pid)
        atts <- summaries(pid)
      yield
        assertEquals(jobs.map(j => (j._1, j._2)), List(("failed", 1), ("failed", 1)))
        assert(jobs.forall(_._3.exists(_.contains("Unknown style"))))
        assertEquals(atts, Nil)
  }

}
