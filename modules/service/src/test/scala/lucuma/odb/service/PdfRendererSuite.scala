// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.io.file.PosixPermissions
import fs2.text
import io.circe.Json
import lucuma.odb.data.SummaryStyle
import munit.CatsEffectSuite
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.concurrent.duration.*

/**
 * Exercises the subprocess renderer with a shell stand-in for pyexplore that
 * honours the same arguments and exit codes.
 */
class PdfRendererSuite extends CatsEffectSuite {

  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  // Parses the arguments the renderer passes and behaves per style.
  val stub: String =
    """|#!/bin/sh
       |while [ $# -gt 0 ]; do
       |  case "$1" in
       |    --payload) payload=$2; shift;;
       |    --style)   style=$2;   shift;;
       |    --output)  output=$2;  shift;;
       |  esac
       |  shift
       |done
       |case "$style" in
       |  chile)        echo "ValueError: Unknown style 'chile'" >&2; exit 2;;
       |  noirlab-darp) echo "requests.ConnectionError: ITC unreachable" >&2; exit 1;;
       |  gemini-darp)  sleep 30;;
       |esac
       |cp "$payload" "$output"
       |""".stripMargin

  val payload: Json = Json.obj("schemaVersion" -> Json.fromString("1.0.0"))

  // A temp dir holding the executable stub and the output path.
  val fixture: Resource[IO, (Path, Path)] =
    Files[IO].tempDirectory.evalMap: dir =>
      val script = dir / "render.sh"
      Stream.emit(stub).through(text.utf8.encode).through(Files[IO].writeAll(script)).compile.drain *>
        Files[IO].setPosixPermissions(script, PosixPermissions.fromString("rwxr-xr-x").get).as((script, dir / "out.pdf"))

  def render(style: SummaryStyle, timeout: FiniteDuration = 10.seconds): IO[(Either[PdfRenderer.Error, Unit], Option[String])] =
    fixture.use: (script, out) =>
      for
        r <- PdfRenderer.subprocess[IO](script.toString, timeout).render(payload, style, out)
        o <- Files[IO].exists(out).ifM(Files[IO].readUtf8(out).compile.string.map(_.some), none.pure[IO])
      yield (r, o)

  test("a successful render writes the output file") {
    render(SummaryStyle.GeminiStandard).map: (r, out) =>
      assertEquals(r, ().asRight)
      assertEquals(out, payload.noSpaces.some)
  }

  test("exit code 2 is a permanent failure carrying stderr") {
    render(SummaryStyle.Chile).map: (r, out) =>
      assertEquals(r, PdfRenderer.Error("Renderer exited with code 2: ValueError: Unknown style 'chile'", permanent = true).asLeft)
      assertEquals(out, none)
  }

  test("any other exit code is a transient failure") {
    render(SummaryStyle.NoirlabDarp).map: (r, _) =>
      assertEquals(r, PdfRenderer.Error("Renderer exited with code 1: requests.ConnectionError: ITC unreachable", permanent = false).asLeft)
  }

  test("a render that exceeds the timeout is killed and reported as transient") {
    render(SummaryStyle.GeminiDarp, timeout = 1.second).map: (r, out) =>
      assertEquals(r, PdfRenderer.Error("Renderer did not finish within 1 second", permanent = false).asLeft)
      assertEquals(out, none)
  }

}
