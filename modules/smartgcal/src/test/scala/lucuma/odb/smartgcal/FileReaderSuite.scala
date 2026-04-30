// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.parse.Parser
import cats.syntax.show.*
import fs2.Stream
import lucuma.odb.smartgcal.data.Gmos.FileEntry as GmosFileEntry
import lucuma.odb.smartgcal.parsers.Availability

final class FileReaderSuite extends munit.FunSuite:

  private def loadInstrument[A](
    filename:        String,
    entryParser:     String => Either[Parser.Error, Availability[A]],
    headerLineCount: Int = 2
  ): Unit =

    val r  = s"smartgcal/$filename.csv"
    val is = IO(this.getClass.getClassLoader.getResourceAsStream(r))
    val s  = fs2.io.readInputStream[IO](is,8192)

    val (obsolete, current) =
       s.through(FileReader.entryLines(headerLineCount))
        .flatMap: (lineNumber, s) =>
          entryParser(s) match
            case Left(e)  => Stream.raiseError[IO](new RuntimeException(s"$filename $lineNumber:\n${e.show}"))
            case Right(a) => Stream.emit(a)
        .fold((0L, 0L)) { case ((o, c), a) =>
          a.fold((o+1, c), _ => (o, c+1))
        }
        .compile
        .onlyOrError
        .unsafeRunSync()

    val rawLineCount     = s.through(FileReader.entryLines(headerLineCount)).compile.count.unsafeRunSync()

    // In reality, the test is just that the parsing doesn't fail.  Assuming
    // there are no parse errors the parsed entry count should be the same as
    // the number of entry lines.
    assertEquals((obsolete + current), rawLineCount)

  private def loadF2(filename: String): Unit =
    loadInstrument(filename, parsers.flamingos2.fileEntry.map(Availability.Current.apply).parseAll)

  private def loadGhost(filename: String): Unit =
    loadInstrument(filename, parsers.ghost.row.map(Availability.Current.apply).parseAll, headerLineCount = 1)

  private def loadGmos[G, L, U](
    filename:    String,
    entryParser: Parser[Availability[GmosFileEntry[G, L, U]]]
  ): Unit =
    loadInstrument(filename, entryParser.parseAll)

  test("Flamingos2_ARC"):
    loadF2("Flamingos2_ARC")

  test("Flamingos2_FLAT"):
    loadF2("Flamingos2_FLAT")

  test("GHOST_ARC"):
    loadGhost("GHOST_ARC")

  test("GHOST_FLAT"):
    loadGhost("GHOST_FLAT")

  test("GMOS-N_ARC"):
    loadGmos("GMOS-N_ARC", parsers.gmosNorth.fileEntry)

  test("GMOS-N_FLAT"):
    loadGmos("GMOS-N_FLAT", parsers.gmosNorth.fileEntry)

  test("GMOS-S_ARC"):
    loadGmos("GMOS-S_ARC", parsers.gmosSouth.fileEntry)

  test("GMOS-S_FLAT"):
    loadGmos("GMOS-S_FLAT", parsers.gmosSouth.fileEntry)