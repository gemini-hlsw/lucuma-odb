// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

/**
 * Repeatable Smart GCal configuration loader for Flamingos 2.
 * Located, instantiated and executed by flyway.
 */
class R__SmartFlamingos2 extends SmartGcalMigration("Flamingos2"):

  override val importForcingVersion: Int = 1

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("Flamingos2_ARC", "Flamingos2_FLAT")

  override def ioMigrate(ctx: Context, bc:  BaseConnection): IO[Unit] =
    SmartFlamingos2Loader.F2.load(bc, definitionFiles)
