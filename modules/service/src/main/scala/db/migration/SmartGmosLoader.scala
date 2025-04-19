// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.Instrument.GmosNorth
import lucuma.core.enums.Instrument.GmosSouth
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.Gmos.FileEntry
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GmosCodecs.*
import skunk.Encoder
import skunk.codec.temporal.interval

object SmartGmosLoader {

  import SmartGcalTable.Col

  given Encoder[SmartGcalValue.LegacyInstrumentConfig] =
    interval.contramap[SmartGcalValue.LegacyInstrumentConfig] { v =>
      v.exposureTime.toDuration
    }

  given Encoder[SmartGcalValue.Legacy] =
    SmartGcalTable.valueEncoder

  def keyEncoder[G, L, U](g: Encoder[G], l: Encoder[L], u: Encoder[U]) : Encoder[TableKey[G, L, U]] =
    (
      g.opt                    *:
      l.opt                    *:
      u.opt                    *:
      gmos_x_binning           *:
      gmos_y_binning           *:
      wavelength_pm_range.opt  *:
      gmos_grating_order.opt   *:
      gmos_amp_gain
    ).contramap[TableKey[G, L, U]] { k => (
      k.grating         ,
      k.filter          ,
      k.fpu             ,
      k.xBin            ,
      k.yBin            ,
      k.wavelengthRange ,
      k.order           ,
      k.gain
    )}

  given Encoder[TableKey.North] =
    keyEncoder(gmos_north_grating, gmos_north_filter, gmos_north_fpu)

  given Encoder[TableKey.South] =
    keyEncoder(gmos_south_grating, gmos_south_filter, gmos_south_fpu)

  private def keyColumns(site: String): NonEmptyList[Col] =
    NonEmptyList.of(
      Col.fkey("c_disperser", s"t_gmos_${site}_disperser").index,
      Col.fkey("c_filter", s"t_gmos_${site}_filter").index,
      Col.fkey("c_fpu", s"t_gmos_${site}_fpu").index,
      Col.fkey("c_x_binning", "t_gmos_binning"),
      Col.fkey("c_y_binning", "t_gmos_binning"),
      Col("c_wavelength_range", "d_wavelength_pm_range"),
      Col.fkey("c_disperser_order", "t_gmos_disperser_order"),
      Col.fkey("c_amp_gain", "t_gmos_amp_gain"),
    )

  def encoder[G, L, U](using k: Encoder[TableKey[G, L, U]], v: Encoder[SmartGcalValue.Legacy]): Encoder[TableRow[G, L, U]] =
    (
      int8_pos *:
      k        *:
      v
    ).contramap[TableRow[G, L, U]] { r => (
      r.line ,
      r.key  ,
      r.value
    )}

  val (tmpS, instS) = SmartGcalTable.forInstrument(
    GmosSouth,
    Col("c_step_order", "int8"),
    keyColumns("south"),
    NonEmptyList.one(Col("c_exposure_time", "interval"))
  )

  object South extends SmartGcalLoader(
    tmpS,
    instS,
    pipe    = filename => FileReader.gmosSouth[IO](filename) andThen FileEntry.tableRowsSouth[IO],
    encoder = encoder[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]
  )

  val (tmpN, instN) = SmartGcalTable.forInstrument(
    GmosNorth,
    Col("c_step_order", "int8"),
    keyColumns("north"),
    NonEmptyList.one(Col("c_exposure_time", "interval"))
  )

  object North extends SmartGcalLoader(
    tmpN,
    instN,
    pipe    = filename => FileReader.gmosNorth[IO](filename) andThen FileEntry.tableRowsNorth[IO],
    encoder = encoder[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
  )
}
