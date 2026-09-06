// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
import cats.effect.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.process.ProcessBuilder
import fs2.io.process.Processes
import fs2.text
import io.circe.Json
import lucuma.odb.data.SummaryStyle
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

/** Renders a proposal-summary payload (see `PdfSummaryJobPayload`) to a PDF file. */
trait PdfRenderer[F[_]]:
  def render(payload: Json, style: SummaryStyle, output: Path): F[Either[PdfRenderer.Error, Unit]]

object PdfRenderer:

  /** A failed render; `permanent` failures are not retried. */
  case class Error(message: String, permanent: Boolean)

  // Exit code pyexplore uses for a payload it can never render (bad schema
  // version, unknown style).  Not 2: argparse exits 2 on a usage error.
  val PermanentExitCode: Int = 3

  // How much of the renderer's stderr is kept as the job's error message.
  val MaxErrorLength: Int = 2000

  // Its own logger so pyexplore's chatter can be turned up without turning up the daemon's.
  val LoggerName: String = "pdf-summary-renderer"

  /** For tests: always succeeds, writing `content` to the output file. */
  def constant[F[_]: {Async, Files}](content: String): PdfRenderer[F] =
    (_, _, output) =>
      Stream.emit(content).through(text.utf8.encode).through(Files[F].writeAll(output)).compile.drain.map(_.asRight)

  /** For tests: always fails. */
  def failing[F[_]: Async](error: Error): PdfRenderer[F] =
    (_, _, _) => error.asLeft.pure[F]

  /**
   * Runs `python -m pyexplore.pdf.render` in this container.  The payload is
   * handed over as a file so a large one never hits argument limits, and the
   * process is killed when `timeout` passes.
   */
  def subprocess[F[_]: {Async, Files, Processes, LoggerFactory as LF}](
    python:    String,
    timeout:   FiniteDuration,
    keepFiles: Boolean = false
  ): PdfRenderer[F] =
    given Logger[F] = LF.getLoggerFromName(LoggerName)

    // Kept payloads let a failed render be reproduced by hand:
    // `python -m pyexplore.pdf.render --payload <file> --style ... --output ...`
    val payloadFileResource: Resource[F, Path] =
      if keepFiles then
        Resource.eval(Files[F].createTempFile(None, "summary-payload-", ".json", None))
          .evalTap(f => Logger[F].info(s"Keeping the renderer payload at $f"))
      else Files[F].tempFile(None, "summary-payload-", ".json", None)

    (payload, style, output) =>
      payloadFileResource.use: payloadFile =>
        val write =
          Stream.emit(payload.noSpaces).through(text.utf8.encode).through(Files[F].writeAll(payloadFile)).compile.drain

        val run =
          ProcessBuilder(
            python,
            List(
              "-m", "pyexplore.pdf.render",
              "--payload", payloadFile.toString,
              "--style", style.rendererName,
              "--output", output.toString
            )
          ).spawn[F].use: p =>
            p.stderr.through(text.utf8.decode).compile.string.both(p.exitValue)

        // pyexplore logs to stderr at INFO; keep it on success too, since that
        // is where it says what it left out of the PDF.
        def logOutput(stderr: String): F[Unit] =
          Logger[F].debug(s"Renderer output for $output:\n${stderr.trim}").whenA(stderr.trim.nonEmpty)

        write *> run.timeout(timeout).flatTap((stderr, _) => logOutput(stderr)).map:
          case (_, 0)         => ().asRight
          case (stderr, code) => Error(trim(stderr, code), code === PermanentExitCode).asLeft
        .recover:
          case _: TimeoutException => Error(s"Renderer did not finish within $timeout", permanent = false).asLeft

  private def trim(stderr: String, exitCode: Int): String =
    val text = stderr.trim
    val tail = if text.length <= MaxErrorLength then text else "..." + text.takeRight(MaxErrorLength)
    if tail.isEmpty then s"Renderer exited with code $exitCode" else s"Renderer exited with code $exitCode: $tail"
