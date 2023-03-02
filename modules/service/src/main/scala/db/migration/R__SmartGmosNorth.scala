// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.Resource
import eu.timepit.refined.types.numeric.PosInt
import fs2.Pipe
import java.io.InputStream
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.GmosNorth.FileEntry
import lucuma.odb.smartgcal.data.GmosNorth.TableRow
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.Codec
import skunk.Encoder
import skunk.implicits.*
import skunk.codec.temporal.interval

class R__SmartGmosNorth extends SmartGcalMigration[TableRow]("GMOS North") {

  val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("GMOS-N_ARC", "GMOS-N_FLAT")

  val loader: SmartGcalLoader[TableRow] =
    R__SmartGmosNorth.Loader

}

object R__SmartGmosNorth {

  val encoder: Encoder[TableRow] =
    (
      pos_int                 ~
      step_config_gcal        ~
      interval                ~
      gcal_baseline           ~
      gmos_north_grating.opt  ~
      gmos_north_filter.opt   ~
      gmos_north_fpu.opt      ~
      gmos_x_binning          ~
      gmos_y_binning          ~
      wavelength_pm_range     ~
      gmos_disperser_order      ~
      gmos_amp_gain
    ).contramap[TableRow] { row =>
      PosInt.unsafeFrom(1)    ~
      row.value.gcalConfig    ~
      row.value.instrumentConfig.exposureTime.toDuration  ~
      row.value.baselineType  ~
      row.key.grating         ~
      row.key.filter          ~
      row.key.fpu             ~
      row.key.xBin            ~
      row.key.yBin            ~
      row.key.wavelengthRange ~
      row.key.order           ~
      row.key.gain
    }

  object Loader extends SmartGcalLoader(
    relation          = "smart_gmos_north",
    instrumentColumns = List(
      "c_exposure_time",
      "c_gcal_baseline",
      "c_disperser",
      "c_filter",
      "c_fpu",
      "c_x_binning",
      "c_y_binning",
      "c_wavelength_range",
      "c_disperser_order",
      "c_amp_gain"
    ),
    indexColumns = List("c_disperser", "c_filter", "c_fpu"),
    pipe         = s => FileReader.gmosNorth[IO](s) andThen FileEntry.tableRows[IO],
    encoder      = encoder

  )

}
