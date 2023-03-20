// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import fs2.Pipe
import fs2.Stream
import org.postgresql.core.BaseConnection
import skunk.Encoder

import java.io.InputStream

/**
 * Loads a Smart GCal configuration file into the corresponding table.
 *
 * @param table simple description of the database table
 * @param pipe a function that takes a file name and produces a pipe Byte to
 *             table rows
 * @param encoder encodes a table row for ingestion into the table via copy from
 *                stdin
 * @tparam A scala data type representing the smart gcal table row for the
 *           instrument
 */
class SmartGcalLoader[A](
  val temp:    SmartGcalTable.Temp,
  val inst:    SmartGcalTable.Inst,
  val pipe:    String => Pipe[IO, Byte, A],
  val encoder: Encoder[A]
) {

  def load(bc: BaseConnection, files: NonEmptyList[(String, IO[InputStream])]): IO[Unit] = {

    val r: Resource[IO, InputStream] =
      files
        .map { case (name, is) => fs2.io.readInputStream(is, ByteChunkSize, closeAfterUse = true).through(pipe(name)) }
        .reduce
        .map(a => encoder.encode(a).map(_.getOrElse("NULL")).intercalate("|"))
        .append(Stream("\\.\n"))
        .intersperse("\n")
        .through(fs2.text.utf8.encode)
        .through(fs2.io.toInputStream[IO])
        .compile
        .resource
        .lastOrError

    for {

      // Create and load the temp table
      _ <- bc.ioUpdate(temp.create)
      _ <- bc.ioCopyIn(temp.copyFromStdin, r)

      // Prepare and clean the instrument table
      _ <- bc.ioUpdate(inst.truncate)

      // Insert into t_gcal table from temp
      _ <- bc.ioUpdate(SmartGcalTable.Gcal.deleteWhereInstrumentEquals(inst.inst))
      _ <- bc.ioUpdate(SmartGcalTable.Gcal.dropFkeyConstraints)
      _ <- bc.ioUpdate(SmartGcalTable.Gcal.insertFromTemp(temp.name))
      _ <- bc.ioUpdate(SmartGcalTable.Gcal.addFkeyConstraints)

      // Insert into instrument table
      _ <- bc.ioUpdate(inst.dropIndex)
      _ <- bc.ioUpdate(inst.dropFkeyConstraints)
      _ <- bc.ioUpdate(inst.insertFromTemp(temp.name))
      _ <- bc.ioUpdate(inst.addFkeyConstraints)
      _ <- bc.ioUpdate(inst.createIndex)

    } yield ()
  }

}