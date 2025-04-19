// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

/**
 * Repeatable Smart GCal configuration loader for GMOS North.  Located,
 * instantiated and executed by flyway.
 */
class R__SmartGmosNorth extends SmartGcalMigration("GMOS North"):

  override val importForcingVersion: Int = 1

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("GMOS-N_ARC", "GMOS-N_FLAT")

  override def ioMigrate(ctx: Context, bc:  BaseConnection): IO[Unit] =
    SmartGmosLoader.North.load(bc, definitionFiles)