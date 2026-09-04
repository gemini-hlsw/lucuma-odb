// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.Queue
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import lucuma.odb.service.Services.Syntax.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.syntax.*
import org.typelevel.otel4s.trace.Tracer
import skunk.Session
import skunk.syntax.all.*

import scala.concurrent.duration.*

/**
 * Drives proposal-summary PDF jobs end to end: takes pending jobs one at a
 * time, renders each with pyexplore, uploads the PDF and records the
 * attachment, or records the failure.  It wakes on `ch_summary_job`
 * notifications and also polls, so a lost notification only delays a job by
 * one poll period.
 *
 * The `services` must carry a GraphQL mapping (see `OdbMapping.forObscalc`):
 * building the payload runs a GraphQL query through it.
 */
object PdfSummaryJobDaemon:

  val Channel = id"ch_summary_job"

  def run[F[_]: {Async, Files, LoggerFactory as LF, Tracer as T}](
    pollPeriod: FiniteDuration,
    session:    Resource[F, Session[F]],
    services:   Resource[F, Services[F]],
    renderer:   PdfRenderer[F],
    keepFiles:  Boolean = false
  ): Resource[F, Unit] =
    given Logger[F] = LF.getLoggerFromName("pdf-summary-jobs")

    // The PDF is uploaded and then thrown away, so `keepFiles` is the only way
    // to look at what was actually rendered.  Debugging only: nothing sweeps
    // these up, and each one is several megabytes.
    val workspace: Resource[F, fs2.io.file.Path] =
      if keepFiles then
        Resource.eval(Files[F].createTempDirectory)
          .evalTap(d => info"Keeping the rendered PDF in $d")
      else Files[F].tempDirectory

    def logged(what: String)(fa: F[Unit]): F[Unit] =
      fa.handleErrorWith(e => Logger[F].error(e)(s"PDF summary job daemon: $what failed"))

    def next: F[Option[PdfSummaryJobService.Prepared]] =
      services.useNonTransactionally:
        requireServiceAccessOrThrow:
          pdfSummaryJobService.next

    def fail(prepared: PdfSummaryJobService.Prepared, err: PdfRenderer.Error): F[Unit] =
      services.useTransactionally:
        requireServiceAccessOrThrow:
          pdfSummaryJobService.fail(prepared.job, err.message, err.permanent)
      *> Logger[F].warn(s"Summary job ${prepared.job.id} failed (permanent: ${err.permanent}): ${err.message}")

    // Render outside any transaction or pooled session: it takes minutes.
    def render(prepared: PdfSummaryJobService.Prepared): F[Unit] =
      workspace.use: dir =>
        val out = dir / prepared.fileName.value
        renderer.render(prepared.payload, prepared.job.style, out).flatMap:
          case Left(err) => fail(prepared, err)
          case Right(()) =>
            Files[F].size(out).flatMap: bytes =>
              info"Summary job ${prepared.job.id}: uploading ${prepared.fileName} ($bytes bytes) to s3 ${prepared.remotePath}" *>
                services.useNonTransactionally:
                  requireServiceAccessOrThrow:
                    pdfSummaryJobService.finalize(prepared, Files[F].readAll(out))
                *> info"Summary job ${prepared.job.id} done: uploaded to s3 ${prepared.remotePath}, attachment recorded"
      .handleErrorWith: e =>
        fail(prepared, PdfRenderer.Error(s"${e.getClass.getSimpleName}: ${e.getMessage}", permanent = false))

    def renderOne(prepared: PdfSummaryJobService.Prepared): F[Unit] =
      logged(s"rendering job ${prepared.job.id}"):
        T.rootSpan("pdf-summary-job.render").surround:
          info"Summary job ${prepared.job.id}: rendering (attempt ${prepared.job.attempts})" *>
            render(prepared)

    // Takes jobs until nothing is waiting.  A failure to take one ends the
    // drain; the next notification or poll starts another.
    def drain: F[Unit] =
      logged("draining pending jobs"):
        next.flatMap:
          case None           => ().pure[F]
          case Some(prepared) => renderOne(prepared) *> drain

    val daemon: F[Unit] =
      for
        // A one-slot queue coalesces wake-ups so a single drain runs at a time.
        wake <- Queue.bounded[F, Unit](1)
        _    <- wake.offer(())
        events = Stream.resource(session).flatMap(_.channel(Channel).listen(1024)).evalMap(_ => wake.tryOffer(()).void)
        polls  = Stream.awakeEvery(pollPeriod).evalMap(_ => wake.tryOffer(()).void)
        drains = Stream.repeatEval(wake.take *> drain)
        _    <- info"PDF summary job daemon: listening on $Channel, polling every $pollPeriod"
        _    <- Stream(events, polls, drains).parJoinUnbounded.compile.drain
      yield ()

    daemon.background.void
