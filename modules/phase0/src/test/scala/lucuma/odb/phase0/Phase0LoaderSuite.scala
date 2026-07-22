// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import cats.effect.IO
import lucuma.odb.phase0.FileReader
import lucuma.odb.phase0.FpuOption
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
        assertEquals(rows.length, 322)
        assertEquals(rows.count(_._1.spec.fpuOption == FpuOption.Multislit), 161)
        assertEquals(rows.count(_._1.spec.fpuOption == FpuOption.Singleslit), 161)

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
        assertEquals(rows.length, 364)
        assertEquals(rows.count(_._1.spec.fpuOption == FpuOption.Multislit), 182)

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

  test("loadAll GHOST IFU configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.ghostIfu)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 2)

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

  test("loadAll gnirs spectroscopy configurations"):
    val rdr = FileReader[IO](fileName)
    val inputStream = getClass.getResourceAsStream(fileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gnirsSpectroscopy)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 229)

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

  test("loadAll flamingos2 imaging configurations"):
    val rdr = FileReader[IO](imgFileName)
    val inputStream = getClass.getResourceAsStream(imgFileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.flamingos2Imaging)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 6)

  test("loadAll gnirs imaging configurations"):
    val rdr = FileReader[IO](imgFileName)
    val inputStream = getClass.getResourceAsStream(imgFileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.gnirsImaging)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 10)

  test("load alopeke configurations"):
    val rdr = FileReader[IO](imgFileName)
    val inputStream = getClass.getResourceAsStream(imgFileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.alopekeImaging)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 2)

  test("load zorro configurations"):
    val rdr = FileReader[IO](imgFileName)
    val inputStream = getClass.getResourceAsStream(imgFileName)
    val stream =
      fs2.io.readInputStream(
        IO(inputStream),
        chunkSize = 4096,
        closeAfterUse = true
      )

    stream
      .through(rdr.zorroImaging)
      .compile
      .toList
      .map: rows =>
        assertEquals(rows.length, 2)
