// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection

import java.io.InputStream

/**
 * A repeatable migration that is executed whenver the input .tsv file is
 * changed.
 */
class R__Phase0 extends RepeatableMigration("Phase 0 Instrument Options") {

  val spectroscopyFileName: String = "Phase0_Instrument_Matrix - Spectroscopy.tsv"

  val imagingFileName: String = "Phase0_Instrument_Matrix - Imaging.tsv"

  override val importForcingVersion: Int = 6

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    filesFromClasspath("phase0", NonEmptyList.of(spectroscopyFileName, imagingFileName))

  override def ioMigrate(ctx: Context, bc: BaseConnection): IO[Unit] =
    definitionFiles.map(_._2).toList match {
      case spec :: imaging :: Nil =>
        Phase0Loader.spectroscopyLoadAll(bc, spectroscopyFileName, spec) *>
          Phase0Loader.imagingLoadAll(bc, imagingFileName, imaging)
      case _ =>
        IO.raiseError(
          new RuntimeException(
            s"Expected 2 input files, found ${definitionFiles.size}"
          )
        )
    }

}
