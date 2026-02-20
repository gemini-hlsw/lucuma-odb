// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import cats.effect.IO
import lucuma.odb.phase0.FileReader
import munit.CatsEffectSuite

class Phase0LoaderSuite extends CatsEffectSuite:
  val fileName = "/phase0/Phase0_Instrument_Matrix - Spectroscopy.tsv"
  val imgFileName = "/phase0/Phase0_Instrument_Matrix - Imaging.tsv"

  test("loadAll gmosNorth spectroscopy configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gmosNorthSpectroscopy)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 161)

  test("loadAll gmosSouth spectroscopy configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gmosSouthSpectroscopy)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 182)

  test("loadAll flamingos2 spectroscopy configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.flamingos2Spectroscopy)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 42)

  test("loadAll igrins2 spectroscopy configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.igrins2Spectroscopy)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 1)

  test("loadAll gmosNorth imaging configurations"):
    val rdr = FileReader[IO](imgFileName)
    val inputStream = getClass.getResourceAsStream(imgFileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gmosNorthImaging)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 23)

  test("loadAll gmosSouth imaging configurations"):
    val rdr = FileReader[IO](imgFileName)
    val inputStream = getClass.getResourceAsStream(imgFileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gmosSouthImaging)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 22)
