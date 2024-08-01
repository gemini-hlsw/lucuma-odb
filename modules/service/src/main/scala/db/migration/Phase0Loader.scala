// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.foldable.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Pipe
import fs2.Stream
import fs2.io.readInputStream
import lucuma.core.enums.Instrument
import lucuma.odb.phase0.FileReader
import lucuma.odb.phase0.GmosSpectroscopyRow
import lucuma.odb.phase0.SpectroscopyRow
import org.postgresql.core.BaseConnection

import java.io.InputStream

/**
 * Loads a Phase 0 configuration file into the corresponding table.
 */
class Phase0Loader[A](
  val instrument: Instrument,
  val pipe:       Pipe[IO, Byte, (A, PosInt)],
  val specRow:    A => SpectroscopyRow,
  val instTable:  Phase0Table[A]
) {

  def load(bc: BaseConnection, is: IO[InputStream]): IO[Unit] = {

    def toInputStream(s: Stream[IO, String]): Resource[IO, InputStream] =
      s.append(Stream("\\.\n"))
       .intersperse("\n")
       .through(fs2.text.utf8.encode)
       .through(fs2.io.toInputStream[IO])
       .compile
       .resource
       .lastOrError

    val s: Stream[IO, (String, String)] =
      readInputStream(is, ByteChunkSize, closeAfterUse = true)
        .through(pipe)
        .map { (a, idx) => (
          Phase0Table.Spectroscopy.stdinLine(specRow(a), idx),
          instTable.stdinLine(a, idx)
        )}

    for {
      _  <- bc.ioUpdate(instTable.truncate)
      _  <- bc.ioUpdate(Phase0Table.Spectroscopy.deleteFrom(instrument))
      _  <- bc.ioCopyIn(Phase0Table.Spectroscopy.copyFromStdin, toInputStream(s.map(_._1)))
      _  <- bc.ioCopyIn(instTable.copyFromStdin, toInputStream(s.map(_._2)))
    } yield ()

  }

}

object Phase0Loader {

  def loadAll(bc: BaseConnection, fileName: String, is: IO[InputStream]): IO[Unit] =
    val rdr = FileReader[IO](fileName)
    List(
      new Phase0Loader[GmosSpectroscopyRow.GmosNorth](Instrument.GmosNorth, rdr.gmosNorth, _.spec, Phase0Table.SpectroscopyGmosNorth),
      new Phase0Loader[GmosSpectroscopyRow.GmosSouth](Instrument.GmosSouth, rdr.gmosSouth, _.spec, Phase0Table.SpectroscopyGmosSouth)
    ).traverse_(_.load(bc, is))

}