// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.Pipe
import fs2.Stream
import fs2.io.readInputStream
import lucuma.core.enums.Instrument
import lucuma.odb.phase0.ConfigurationRow
import lucuma.odb.phase0.FileReader
import lucuma.odb.phase0.Flamingos2SpectroscopyRow
import lucuma.odb.phase0.GmosImagingRow
import lucuma.odb.phase0.GmosSpectroscopyRow
import lucuma.odb.phase0.ImagingRow
import lucuma.odb.phase0.SpectroscopyRow
import org.postgresql.core.BaseConnection

import java.io.InputStream

enum ConfigModeVariant:
  case Spectroscopy
  case Imaging

/**
 * Loads a Phase 0 configuration file into the corresponding tables.  There are
 * two tables: the general spectroscopy table (where for example the grating,
 * filter and FPU are identified with strings) and the instrument specific
 * spectroscopy table (where instrument specific grating, filter and FPU values
 * are referenced as FKs).
 */
class Phase0Loader[A, B <: ConfigurationRow](
  val instrument: Instrument,
  val pipe:       Pipe[IO, Byte, (A, PosInt)],
  val row:    A => B,
  val instTable:  Option[Phase0Table[A]]
) {

  def load(bc: BaseConnection, mode: ConfigModeVariant, is: IO[InputStream]): IO[Unit] = {

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
          row(a) match {
            case s: SpectroscopyRow =>
              (Phase0Table.Spectroscopy.stdinLine(s, idx))
            case i: ImagingRow =>
              (Phase0Table.Imaging.stdinLine(i, idx))
          },
          instTable.foldMap(_.stdinLine(a, idx))
        )}

    val loader = mode match {
      case ConfigModeVariant.Spectroscopy => Phase0Table.Spectroscopy
      case ConfigModeVariant.Imaging      => Phase0Table.Imaging
    }

    // 1. Truncate the instrument table, it will be replaced with entries from
    //    the .tsv file.
    // 2. Delete from the general spectroscopy table where the instrument
    //    matches.  The general table holds all the entries for all the
    //    instruments so we can't just truncate it.
    // 3. Bulk copy in to the spectroscopy table.
    // 4. Bulk copy in to the instrument table.
    for {
      _  <- IO.println(s"Loading Phase 0 table for $instrument $mode ${instTable.fold("n/a")(_.name)}")
      p0 <- rows.compile.last.map(_.map(_._1))
      _  <- instTable.foldMap(it => bc.ioUpdate(it.truncate))
      _  <- bc.ioUpdate(loader.deleteFrom(instrument))
      _  <- bc.ioCopyIn(loader.copyFromStdin, toInputStream(rows.map(_._1)))
      _  <- instTable.foldMap(it => bc.ioCopyIn(it.copyFromStdin, toInputStream(rows.map(_._2))))
    } yield ()

  }

}

object Phase0Loader {

  def spectroscopyLoadAll(bc: BaseConnection, fileName: String, is: IO[InputStream]): IO[Unit] =
    val rdr = FileReader[IO](fileName)
    List(
      new Phase0Loader[GmosSpectroscopyRow.GmosNorth, SpectroscopyRow](Instrument.GmosNorth, rdr.gmosNorthSpectroscopy, _.spec, Phase0Table.SpectroscopyGmosNorth.some),
      new Phase0Loader[GmosSpectroscopyRow.GmosSouth, SpectroscopyRow](Instrument.GmosSouth, rdr.gmosSouthSpectroscopy, _.spec, Phase0Table.SpectroscopyGmosSouth.some),
      new Phase0Loader[Flamingos2SpectroscopyRow, SpectroscopyRow](Instrument.Flamingos2, rdr.flamingos2Spectroscopy, _.spec, Phase0Table.SpectroscopyFlamingos2.some),
      new Phase0Loader[SpectroscopyRow, SpectroscopyRow](Instrument.Igrins2, rdr.igrins2Spectroscopy, identity, none)
    ).traverse_(_.load(bc, ConfigModeVariant.Spectroscopy, is))

  def imagingLoadAll(bc: BaseConnection, fileName: String, is: IO[InputStream]): IO[Unit] =
    val rdr = FileReader[IO](fileName)
    List(
      new Phase0Loader[GmosImagingRow.GmosNorth, ImagingRow](Instrument.GmosNorth, rdr.gmosNorthImaging, _.img, Phase0Table.ImagingGmosNorth.some),
      new Phase0Loader[GmosImagingRow.GmosSouth, ImagingRow](Instrument.GmosSouth, rdr.gmosSouthImaging, _.img, Phase0Table.ImagingGmosSouth.some),
    ).traverse_(_.load(bc, ConfigModeVariant.Imaging, is))

}
