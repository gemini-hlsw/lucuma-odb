// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import lucuma.odb.smartgcal.data.Gmos.FileEntry

final class FileReaderSuite extends munit.FunSuite {

  private def loadGmos[G, L, U](
    filename: String,
    pipe:     String => Pipe[IO, Byte, (PosLong, FileEntry[G, L, U])]
  ): Unit = {

    val r  = s"smartgcal/$filename.csv"
    val is = IO(this.getClass.getClassLoader.getResourceAsStream(r))
    val s  = fs2.io.readInputStream[IO](is,8192)

    val parsedEntryCount = s.through(pipe(r)).compile.count.unsafeRunSync()
    val rawLineCount     = s.through(FileReader.entryLines).compile.count.unsafeRunSync()

    // In reality, the test is just that the parsing doesn't fail.  Assuming
    // there are no parse errors the parsed entry count should be the same as
    // the number of entry lines.
    assertEquals(parsedEntryCount, rawLineCount)
  }

  test("GMOS-N_ARC") {
    loadGmos("GMOS-N_ARC", FileReader.gmosNorth)
  }

  test("GMOS-N_FLAT") {
    loadGmos("GMOS-N_FLAT", FileReader.gmosNorth)
  }

  test("GMOS-S_ARC") {
    loadGmos("GMOS-S_ARC", FileReader.gmosSouth)
  }

  test("GMOS-S_FLAT") {
    loadGmos("GMOS-S_FLAT", FileReader.gmosSouth)
  }
}
