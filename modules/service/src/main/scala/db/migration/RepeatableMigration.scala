// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import org.flywaydb.core.api.MigrationVersion

import java.io.InputStream
import scala.math.BigInt

/**
 * Performs a file-based repeatable migration which consists of reading the
 * named `definitionFiles`, parsing them and updating the corresponding tables.
 * Most of this work is delegated to loaders which could be used in other
 * contexts.
 *
 * @param name identifies the migration in `getDescription`
  */
abstract class RepeatableMigration(val getDescription: String) extends IOMigration {

  /**
   * Usually the migration is triggered by a change to the import file checksum,
   * but there are occassions where we need to re-import without any changes to the file.
   * Increment this value to force the migration to occur.
   */
  val importForcingVersion: Int

  /**
   * Returns `null` as required by the Flyway API for repeatable migrations.
   * Repeatable migrations are always executed after versioned migrations but
   * in any order thereafter. They are skipped if the checksum (see
   * `getChecksum`) has not been updated since the last time it was executed.
   */
  override val getVersion: MigrationVersion =
    null // required by the API for repeatable migrations

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
        .append(Stream.evalSeq(IO.pure(BigInt(importForcingVersion).toByteArray.toList)))
        .through(fs2.compression.checksum.crc32)
        .compile
        .to(Array)
        .unsafeRunSync()
    
    BigInt(bytes).intValue
  }

  /**
   * List of the definition files.
   */
  def definitionFiles: NonEmptyList[(String, IO[InputStream])]

}
