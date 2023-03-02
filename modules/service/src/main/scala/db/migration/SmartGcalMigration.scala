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
import java.io.InputStream
import java.time.Duration
import java.time.Instant
import lucuma.odb.smartgcal.FileReader
import org.flywaydb.core.api.MigrationVersion
import org.flywaydb.core.api.migration.Context
import org.flywaydb.core.api.migration.JavaMigration
import org.postgresql.core.BaseConnection
import scala.math.BigInt
import skunk.Codec

abstract class SmartGcalMigration[A](instrumentName: String) extends IOMigration {

  override val getVersion: MigrationVersion =
    null // required by the API for repeatable migrations

  override val getDescription: String =
    s"${instrumentName} smart gcal loader"

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

/*
TRUNCATE TABLE t_smart_gmos_north RESTART IDENTITY;

DROP INDEX IF EXISTS i_smart_gmos_north;

COPY t_smart_gmos_north (
  c_step_index,
  c_gcal_continuum,
  c_gcal_ar_arc,
  c_gcal_cuar_arc,
  c_gcal_thar_arc,
  c_gcal_xe_arc,
  c_gcal_filter,
  c_gcal_diffuser,
  c_gcal_shutter,
  c_exposure_time,
  c_gcal_baseline,
  c_disperser,
  c_filter,
  c_fpu,
  c_x_binning,
  c_y_binning,
  c_wavelength_range,
  c_amp_gain
) FROM STDIN WITH (DELIMITER '|', NULL 'NULL');
0|IrGreyBodyLow|f|f|f|f|Gmos|Ir|Open|5 seconds|Day|B1200_G5301|GPrime|LongSlit_0_25|One|One|[600, 1000)|Low
\.

CREATE INDEX i_smart_gmos_north ON t_smart_gmos_north (
  c_disperser,
  c_filter,
  c_fpu
);
*/
