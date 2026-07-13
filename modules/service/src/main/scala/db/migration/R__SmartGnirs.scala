// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

class R__SmartGnirs extends SmartGcalMigration("Gnirs"):

  override val importForcingVersion: Int = 3

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("GNIRS_ARC", "GNIRS_FLAT")

  override def ioMigrate(ctx: Context, bc: BaseConnection): IO[Unit] =
    SmartGnirsLoader.Gn.load(bc, definitionFiles)
