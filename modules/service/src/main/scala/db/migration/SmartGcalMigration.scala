// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.flywaydb.core.api.MigrationVersion

import java.io.InputStream
import scala.math.BigInt

/**
 * Performs a smart gcal migration which consists of reading the named
 * `definitionFiles`, parsing them and updating the corresponding tables.
 * Most of this work is delegated to a `SmartGcalLoader` which could be used
 * in other contexts.
 *
 * @param instrumentName identifies the migration in `getDescription`
  */
abstract class SmartGcalMigration(instrumentName: String) extends IOMigration {

  /**
   * Returns `null` as required by the Flyway API for repeatable migrations.
   * Repeatable migrations are always executed after versioned migrations but
   * in any order thereafter. They are skipped if the checksum (see
   * `getChecksum`) has not been updated since the last time it was executed.
   */
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

  /**
   * List of the definition files that describe the SmartGcal definitions for
   * this instrument.
   */
  def definitionFiles: NonEmptyList[(String, IO[InputStream])]

}