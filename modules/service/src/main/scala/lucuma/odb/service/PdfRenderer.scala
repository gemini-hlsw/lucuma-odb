// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Async
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

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration

/** Renders a proposal-summary payload (see `PdfSummaryJobPayload`) to a PDF file. */
trait PdfRenderer[F[_]]:
  def render(payload: Json, style: SummaryStyle, output: Path): F[Either[PdfRenderer.Error, Unit]]

object PdfRenderer:

  /** A failed render; `permanent` failures are not retried. */
  case class Error(message: String, permanent: Boolean)

  // Exit code pyexplore uses for a payload it can never render (bad schema version, unknown style).
  val PermanentExitCode: Int = 2

  // How much of the renderer's stderr is kept as the job's error message.
  val MaxErrorLength: Int = 2000

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
  def subprocess[F[_]: {Async, Files, Processes}](python: String, timeout: FiniteDuration): PdfRenderer[F] =
    (payload, style, output) =>
      Files[F].tempFile(None, "summary-payload-", ".json", None).use: payloadFile =>
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

        write *> run.timeout(timeout).map:
          case (_, 0)         => ().asRight
          case (stderr, code) => Error(trim(stderr, code), code === PermanentExitCode).asLeft
        .recover:
          case _: TimeoutException => Error(s"Renderer did not finish within $timeout", permanent = false).asLeft

  private def trim(stderr: String, exitCode: Int): String =
    val text = stderr.trim
    val tail = if text.length <= MaxErrorLength then text else "..." + text.takeRight(MaxErrorLength)
    if tail.isEmpty then s"Renderer exited with code $exitCode" else s"Renderer exited with code $exitCode: $tail"
