// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

class R__Phase0 extends RepeatableMigration("Phase 0 Instrument Options") {

  val fileName: String = "Phase0_Instrument_Matrix - Spectroscopy.tsv"

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    filesFromClasspath("phase0", NonEmptyList.one(fileName))

  override def ioMigrate(ctx: Context, bc: BaseConnection): IO[Unit] =
    Phase0Loader.loadAll(bc, fileName, definitionFiles.head._2)

}
