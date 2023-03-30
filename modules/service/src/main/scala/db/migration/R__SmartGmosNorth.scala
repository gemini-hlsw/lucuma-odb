// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Instrument.GmosNorth
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.GmosNorth.FileEntry
import lucuma.odb.smartgcal.data.GmosNorth.TableKey
import lucuma.odb.smartgcal.data.GmosNorth.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import org.flywaydb.core.api.migration.Context
import org.postgresql.core.BaseConnection
import skunk.Encoder
import skunk.codec.temporal.interval
import skunk.implicits.*

import java.io.InputStream

/**
 * Repeatable Smart GCal configuration loader for GMOS North.  Located,
 * instantiated and executed by flyway.
 */
class R__SmartGmosNorth extends SmartGcalMigration("GMOS North") {

  lazy val definitionFiles: NonEmptyList[(String, IO[InputStream])] =
    gcalFilesFromClasspath("GMOS-N_ARC", "GMOS-N_FLAT")

  override def ioMigrate(ctx: Context, bc:  BaseConnection): IO[Unit] =
    R__SmartGmosNorth.Loader.load(bc, definitionFiles)

}

object R__SmartGmosNorth {

  import SmartGcalTable.Col

  given Encoder[SmartGcalValue.LegacyInstrumentConfig] =
    interval.contramap[SmartGcalValue.LegacyInstrumentConfig] { v =>
      v.exposureTime.toDuration
    }

  given Encoder[SmartGcalValue.Legacy] =
    SmartGcalTable.valueEncoder

  given Encoder[TableKey] =
    (
      gmos_north_grating.opt   ~
      gmos_north_filter.opt    ~
      gmos_north_fpu.opt       ~
      gmos_x_binning           ~
      gmos_y_binning           ~
      wavelength_pm_range.opt  ~
      gmos_disperser_order.opt ~
      gmos_amp_gain
    ).contramap[TableKey] { k =>
      k.grating         ~
      k.filter          ~
      k.fpu             ~
      k.xBin            ~
      k.yBin            ~
      k.wavelengthRange ~
      k.order           ~
      k.gain
    }

  val KeyColumns: NonEmptyList[Col] =
    NonEmptyList.of(
      Col.fkey("c_disperser", "t_gmos_north_disperser").index,
      Col.fkey("c_filter", "t_gmos_north_filter").index,
      Col.fkey("c_fpu", "t_gmos_north_fpu").index,
      Col.fkey("c_x_binning", "t_gmos_binning"),
      Col.fkey("c_y_binning", "t_gmos_binning"),
      Col("c_wavelength_range", "d_wavelength_pm_range"),
      Col.fkey("c_disperser_order", "t_gmos_disperser_order"),
      Col.fkey("c_amp_gain", "t_gmos_amp_gain"),
    )

  def encoder(using k: Encoder[TableKey], v: Encoder[SmartGcalValue.Legacy]): Encoder[TableRow] =
    (
      pos_long ~
      k        ~
      v
    ).contramap[TableRow] { r =>
      r.line ~
      r.key  ~
      r.value
    }

  val (tmp, inst) = SmartGcalTable.forInstrument(
    GmosNorth,
    Col("c_step_order", "int8"),
    KeyColumns,
    NonEmptyList.one(Col("c_exposure_time", "interval"))
  )

  object Loader extends SmartGcalLoader(
    tmp,
    inst,
    pipe    = filename => FileReader.gmosNorth[IO](filename) andThen FileEntry.tableRows[IO],
    encoder = encoder
  )

}
