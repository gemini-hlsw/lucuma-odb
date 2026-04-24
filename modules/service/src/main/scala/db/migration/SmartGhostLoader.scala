// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import eu.timepit.refined.types.numeric.PosLong
import lucuma.core.enums.Instrument.Ghost
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.Ghost.GhostConfig
import lucuma.odb.smartgcal.data.Ghost.TableKey
import lucuma.odb.smartgcal.data.Ghost.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.GhostCodecs.*
import skunk.Encoder

object SmartGhostLoader:

  import SmartGcalTable.Col

  given Encoder[TableKey] =
    (ghost_resolution_mode *: ghost_binning).contramap: k =>
      (
        k.resolutionMode,
        k.binning
      )

  given Encoder[GhostConfig] =
    (
      time_span *:
      int4_pos  *:
      time_span *:
      int4_pos  *:
      time_span
    ).contramap: c =>
      (
        c.redExposureTime,
        c.redExposureCount,
        c.blueExposureTime,
        c.blueExposureCount,
        c.slitExposureTime
      )

  given Encoder[SmartGcalValue[GhostConfig]] =
    SmartGcalTable.valueEncoder[GhostConfig]

  given (using k: Encoder[TableKey], v: Encoder[SmartGcalValue[GhostConfig]]): Encoder[TableRow] =
    (k *: v).contramap(r => (r.key, r.value))

  def encoder(using r: Encoder[TableRow]): Encoder[(PosLong, TableRow)] =
    (int8_pos *: r)

  val keyColumns: NonEmptyList[Col] =
    NonEmptyList.of(
      Col.fkey("c_resolution_mode", "t_ghost_resolution_mode").index,
      Col.fkey("c_binning", "t_ghost_binning").index
    )

  val valueColumns: NonEmptyList[Col] =
    NonEmptyList.of(
      Col("c_red_exposure_time", "interval"),
      Col("c_red_exposure_count", "int4"),
      Col("c_blue_exposure_time", "interval"),
      Col("c_blue_exposure_count", "int4"),
      Col("c_slit_viewing_camera_exposure_time", "interval")
    )

  val (tmp, inst) = SmartGcalTable.forInstrument(
    Ghost,
    Col("c_step_order", "int8"),
    keyColumns,
    valueColumns
  )

  object GhostLoader extends SmartGcalLoader(
    tmp,
    inst,
    pipe    = filename => FileReader.ghost[IO](filename),
    encoder = encoder
  )