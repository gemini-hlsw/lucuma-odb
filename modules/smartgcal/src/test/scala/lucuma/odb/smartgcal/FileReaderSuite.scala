// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import fs2.Stream

final class FileReaderSuite extends munit.FunSuite:

  // Reads a file through the same pipe its migration uses, so a parse error or
  // a misread header fails here rather than at deploy time.
  private def load[A](filename: String, reader: String => Pipe[IO, Byte, (PosLong, A)]): Unit =
    val is   = IO(this.getClass.getClassLoader.getResourceAsStream(s"smartgcal/$filename.csv"))
    val rows = fs2.io.readInputStream[IO](is, 8192).through(reader(filename)).compile.count.unsafeRunSync()
    assert(rows > 0, s"$filename produced no rows")

  test("Flamingos2_ARC"):
    load("Flamingos2_ARC", FileReader.flamingos2[IO])

  test("Flamingos2_FLAT"):
    load("Flamingos2_FLAT", FileReader.flamingos2[IO])

  test("GHOST_ARC"):
    load("GHOST_ARC", FileReader.ghost[IO])

  test("GHOST_FLAT"):
    load("GHOST_FLAT", FileReader.ghost[IO])

  test("GMOS-N_ARC"):
    load("GMOS-N_ARC", FileReader.gmosNorth[IO])

  test("GMOS-N_FLAT"):
    load("GMOS-N_FLAT", FileReader.gmosNorth[IO])

  test("GMOS-S_ARC"):
    load("GMOS-S_ARC", FileReader.gmosSouth[IO])

  test("GMOS-S_FLAT"):
    load("GMOS-S_FLAT", FileReader.gmosSouth[IO])

  test("GNIRS_ARC"):
    load("GNIRS_ARC", FileReader.gnirs[IO])

  test("GNIRS_FLAT"):
    load("GNIRS_FLAT", FileReader.gnirs[IO])

  test("IGRINS-2_ARC"):
    load("IGRINS-2_ARC", FileReader.igrins2[IO])

  test("IGRINS-2_FLAT"):
    load("IGRINS-2_FLAT", FileReader.igrins2[IO])

  private def entryLines(content: String): Either[Throwable, List[(Long, String)]] =
    Stream.emit(content)
      .through(fs2.text.utf8.encode)
      .through(FileReader.entryLines[IO]("test.csv"))
      .map((n, s) => (n.value, s))
      .compile
      .toList
      .attempt
      .unsafeRunSync()

  private val Header = "Mode,Calibration Observe,Calibration Lamps"

  test("entryLines skips a version line and the header"):
    assertEquals(
      entryLines(s"525,2025/05/30 05:58:27 HST\n$Header\nrow1\nrow2"),
      Right(List(3L -> "row1", 4L -> "row2"))
    )

  test("entryLines keeps the first row when there is no version line"):
    assertEquals(
      entryLines(s"# comment\n#\n$Header\n#\nrow1\nrow2"),
      Right(List(5L -> "row1", 6L -> "row2"))
    )

  test("entryLines rejects a file whose header is missing"):
    entryLines("525,2025/05/30 05:58:27 HST\nrow1\nrow2") match
      case Left(e: FileReader.HeaderException) => assert(e.reason.contains("expected the column header"))
      case other                               => fail(s"expected a HeaderException, got $other")

  test("entryLines rejects an empty file"):
    entryLines("# only a comment\n") match
      case Left(e: FileReader.HeaderException) => assertEquals(e.reason, "no column header found")
      case other                               => fail(s"expected a HeaderException, got $other")
