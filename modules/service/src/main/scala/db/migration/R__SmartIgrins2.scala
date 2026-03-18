// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

class R__SmartIgrins2 extends SmartGcalMigration("Igrins2"):

  override val importForcingVersion: Int = 1

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("IGRINS-2_ARC", "IGRINS-2_FLAT")

  override def ioMigrate(ctx: Context, bc: BaseConnection): IO[Unit] =
    SmartIgrins2Loader.Ig2.load(bc, definitionFiles)
