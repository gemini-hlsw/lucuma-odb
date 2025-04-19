// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.odb.phase0.F2SpectroscopyRow
import lucuma.odb.phase0.FileReader
import lucuma.odb.phase0.GmosSpectroscopyRow
import lucuma.odb.phase0.SpectroscopyRow
import org.postgresql.core.BaseConnection

import java.io.InputStream

/**
 * Loads a Phase 0 configuration file into the corresponding tables.  There are
 * two tables: the general spectroscopy table (where for example the grating,
 * filter and FPU are identified with strings) and the instrument specific
 * spectroscopy table (where instrument specific grating, filter and FPU values
 * are referenced as FKs).
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

    // Each element of the Stream will contain a general spectroscopy table
    // entry and an instrument-specific entry.
    val rows: Stream[IO, (String, String)] =
      readInputStream(is, ByteChunkSize, closeAfterUse = true)
        .through(pipe)
        .map { (a, idx) => (
          Phase0Table.Spectroscopy.stdinLine(specRow(a), idx),
          instTable.stdinLine(a, idx)
        )}

    // 1. Truncate the instrument table, it will be replaced with entries from
    //    the .tsv file.
    // 2. Delete from the general spectroscopy table where the instrument
    //    matches.  The general table holds all the entries for all the
    //    instruments so we can't just truncate it.
    // 3. Bulk copy in to the spectroscopy table.
    // 4. Bulk copy in to the instrument table.
    for {
      _  <- bc.ioUpdate(instTable.truncate)
      _  <- bc.ioUpdate(Phase0Table.Spectroscopy.deleteFrom(instrument))
      _  <- bc.ioCopyIn(Phase0Table.Spectroscopy.copyFromStdin, toInputStream(rows.map(_._1)))
      _  <- bc.ioCopyIn(instTable.copyFromStdin, toInputStream(rows.map(_._2)))
    } yield ()

  }

}

object Phase0Loader {

  def loadAll(bc: BaseConnection, fileName: String, is: IO[InputStream]): IO[Unit] =
    val rdr = FileReader[IO](fileName)
    List(
      new Phase0Loader[GmosSpectroscopyRow.GmosNorth](Instrument.GmosNorth, rdr.gmosNorth, _.spec, Phase0Table.SpectroscopyGmosNorth),
      new Phase0Loader[GmosSpectroscopyRow.GmosSouth](Instrument.GmosSouth, rdr.gmosSouth, _.spec, Phase0Table.SpectroscopyGmosSouth),
      new Phase0Loader[F2SpectroscopyRow](Instrument.Flamingos2, rdr.f2, _.spec, Phase0Table.SpectroscopyF2)
    ).traverse_(_.load(bc, is))

}
