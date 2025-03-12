// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.parse.Parser
import fs2.Stream
import lucuma.odb.smartgcal.data.Gmos.FileEntry
import lucuma.odb.smartgcal.parsers.Availability

final class FileReaderSuite extends munit.FunSuite:

  private def loadGmos[G, L, U](
    filename:    String,
    entryParser: Parser[Availability[FileEntry[G, L, U]]]
  ): Unit =

    val r  = s"smartgcal/$filename.csv"
    val is = IO(this.getClass.getClassLoader.getResourceAsStream(r))
    val s  = fs2.io.readInputStream[IO](is,8192)

    val (obsolete, current) =
       s.through(FileReader.entryLines)
        .flatMap: (lineNumber, s) =>
          entryParser.parseAll(s) match
            case Left(e)  => Stream.raiseError[IO](new RuntimeException(s"$filename $lineNumber: $s"))
            case Right(a) => Stream.emit(a)
        .fold((0L, 0L)) { case ((o, c), a) =>
          a.fold((o+1, c), _ => (o, c+1))
        }
        .compile
        .onlyOrError
        .unsafeRunSync()

    val rawLineCount     = s.through(FileReader.entryLines).compile.count.unsafeRunSync()

    // In reality, the test is just that the parsing doesn't fail.  Assuming
    // there are no parse errors the parsed entry count should be the same as
    // the number of entry lines.
    assertEquals((obsolete + current), rawLineCount)

  test("GMOS-N_ARC"):
    loadGmos("GMOS-N_ARC", parsers.gmosNorth.fileEntry)

  test("GMOS-N_FLAT"):
    loadGmos("GMOS-N_FLAT", parsers.gmosNorth.fileEntry)

  test("GMOS-S_ARC"):
    loadGmos("GMOS-S_ARC", parsers.gmosSouth.fileEntry.map(Availability.Current.apply))

  test("GMOS-S_FLAT"):
    loadGmos("GMOS-S_FLAT", parsers.gmosSouth.fileEntry.map(Availability.Current.apply))