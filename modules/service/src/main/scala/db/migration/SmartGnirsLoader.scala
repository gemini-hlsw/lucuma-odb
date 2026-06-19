// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.Instrument.Gnirs
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.Gnirs.FileEntry
import lucuma.odb.smartgcal.data.Gnirs.TableKey
import lucuma.odb.smartgcal.data.Gnirs.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GnirsCodecs.*
import skunk.Encoder
import skunk.codec.temporal.interval

object SmartGnirsLoader:

  import SmartGcalTable.Col

  given Encoder[SmartGcalValue.LegacyInstrumentConfig] =
    interval.contramap[SmartGcalValue.LegacyInstrumentConfig]:
      _.exposureTime.toDuration

  given Encoder[SmartGcalValue.Legacy] =
    SmartGcalTable.valueEncoder

  given Encoder[TableKey] =
    (
      gnirs_pixel_scale   *:
      gnirs_grating       *:
      gnirs_prism         *:
      wavelength_pm_range *:
      gnirs_fpu_slit.opt  *:
      gnirs_fpu_other.opt *:
      gnirs_well_depth
    ).contramap[TableKey]: k =>
      (k.pixelScale, k.disperser, k.crossDispersed, k.wavelengthRange, GnirsFpu.slit.getOption(k.fpu), GnirsFpu.other.getOption(k.fpu), k.wellDepth)

  def encoder(using k: Encoder[TableKey], v: Encoder[SmartGcalValue.Legacy]): Encoder[TableRow] =
    (
      int8_pos *:
      k        *:
      v
    ).contramap[TableRow]: r =>
      (r.line, r.key, r.value)

  private val keyColumns: NonEmptyList[Col] =
    NonEmptyList.of(
      Col("c_pixel_scale", "varchar"),
      Col.fkey("c_disperser", "t_gnirs_grating").index,
      Col.fkey("c_cross_dispersed", "t_gnirs_prism").index,
      Col("c_wavelength_range", "d_wavelength_pm_range"),
      Col.fkey("c_fpu_slit", "t_gnirs_fpu_slit").index,
      Col("c_fpu_other", "e_gnirs_fpu_other").index,
      Col("c_well_depth", "e_gnirs_well_depth")
    )

  val (tmp, inst) = SmartGcalTable.forInstrument(
    Gnirs,
    Col("c_step_order", "int8"),
    keyColumns,
    NonEmptyList.one(Col("c_exposure_time", "interval"))
  )

  object Gn extends SmartGcalLoader(
    temp    = tmp,
    inst    = inst,
    pipe    = FileReader.gnirs[IO](_).andThen(FileEntry.tableRows[IO]),
    encoder = encoder
  )
