// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

/**
 * Repeatable Smart GCal configuration loader for GMOS South.  Located,
 * instantiated and executed by flyway.
 */
class R__SmartGmosSouth extends SmartGcalMigration("GMOS South"):

  override val importForcingVersion: Int = 1

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("GMOS-S_ARC", "GMOS-S_FLAT")

  override def ioMigrate(ctx: Context, bc:  BaseConnection): IO[Unit] =
    SmartGmosLoader.South.load(bc, definitionFiles)