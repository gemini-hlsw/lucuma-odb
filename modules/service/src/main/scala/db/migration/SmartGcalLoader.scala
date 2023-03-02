// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import cats.syntax.foldable.*
import fs2.Pipe
import fs2.Stream
import java.io.InputStream
import org.postgresql.core.BaseConnection
import skunk.Encoder

class SmartGcalLoader[A](
  val relation:          String,
  val instrumentColumns: List[String],
  val indexColumns:      List[String],
  val pipe:              String => Pipe[IO, Byte, A],
  val encoder:           Encoder[A]
) {

  private val GcalColumns: List[String] =
    List(
      "c_step_index",
      "c_gcal_continuum",
      "c_gcal_ar_arc",
      "c_gcal_cuar_arc",
      "c_gcal_thar_arc",
      "c_gcal_xe_arc",
      "c_gcal_filter",
      "c_gcal_diffuser",
      "c_gcal_shutter"
    )

  lazy val tableName: String =
    s"t_$relation"

  lazy val indexName: String =
    s"i_$relation"

  def load(bc: BaseConnection, files: NonEmptyList[(String, IO[InputStream])]): IO[Unit] = {
    val cols = GcalColumns ++ instrumentColumns

    val r: Resource[IO, InputStream] =
      files
        .map { case (name, is) => fs2.io.readInputStream(is, ByteChunkSize, closeAfterUse = true).through(pipe(name)) }
        .reduce
        .map(a => encoder.encode(a).map(_.getOrElse("NULL")).intercalate("|"))
        .append(Stream("\\.\n"))
        .intersperse("\n")
        .through(fs2.text.utf8.encode)
        .through(fs2.io.toInputStream[IO])
        .compile
        .resource
        .lastOrError

    for {
      _  <- bc.ioUpdate(s"DROP INDEX IF EXISTS $indexName")
      _  <- bc.ioUpdate(s"TRUNCATE TABLE $tableName RESTART IDENTITY")
      _  <- bc.ioCopyIn(s"COPY $tableName ( ${cols.mkString(", ")} ) FROM STDIN WITH ( DELIMITER '|', NULL 'NULL' )", r)
      _  <- bc.ioUpdate(s"CREATE INDEX $indexName ON $tableName ( ${indexColumns.mkString(", ")} )")
    } yield ()
  }

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
