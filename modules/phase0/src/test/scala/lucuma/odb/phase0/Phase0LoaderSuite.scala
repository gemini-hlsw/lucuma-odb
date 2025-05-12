// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import cats.effect.IO
import lucuma.odb.phase0.FileReader
import munit.CatsEffectSuite

class Phase0LoaderSuite extends CatsEffectSuite:
  val fileName = "/phase0/Phase0_Instrument_Matrix - Spectroscopy.tsv"

  test("loadAll gmosNorth configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gmosNorth)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 161)

  test("loadAll gmosSouth configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gmosSouth)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 210)

  test("loadAll flamingos2 configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.f2)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 54)
