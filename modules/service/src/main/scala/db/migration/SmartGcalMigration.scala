// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.IO
import cats.effect.Resource
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import fs2.Pipe
import lucuma.odb.smartgcal.FileReader
import org.flywaydb.core.api.MigrationVersion
import org.flywaydb.core.api.migration.Context
import org.flywaydb.core.api.migration.JavaMigration
import org.postgresql.core.BaseConnection
import skunk.Codec

import java.io.InputStream
import java.time.Duration
import java.time.Instant
import scala.math.BigInt

abstract class SmartGcalMigration[A](instrumentName: String) extends IOMigration {

  override val getVersion: MigrationVersion =
    null // required by the API for repeatable migrations

  override val getDescription: String =
    s"${instrumentName} smart gcal loader"

  /**
   * Computes a CRC checksum such that the migration will be skipped if the
   * definition files have not been updated since the last time the migration
   * was executed.
   */
  override def getChecksum: Integer = {
    val bytes =
      definitionFiles
        .map { case (_, is) => fs2.io.readInputStream(is, ByteChunkSize, closeAfterUse = true) }
        .reduce
        .through(fs2.compression.checksum.crc32)
        .compile
        .to(Array)
        .unsafeRunSync()

    BigInt(bytes).intValue
  }

  def definitionFiles: NonEmptyList[(String, IO[InputStream])]

  def loader: SmartGcalLoader[A]

  def ioMigrate(ctx: Context, bc:  BaseConnection): IO[Unit] =
    loader.load(bc, definitionFiles)

}