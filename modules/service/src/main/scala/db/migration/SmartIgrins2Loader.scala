// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.Instrument.Igrins2
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.Igrins2.FileEntry
import lucuma.odb.smartgcal.data.Igrins2.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import skunk.Encoder
import skunk.codec.temporal.interval

object SmartIgrins2Loader:

  import SmartGcalTable.Col

  given Encoder[SmartGcalValue.LegacyInstrumentConfig] =
    interval.contramap[SmartGcalValue.LegacyInstrumentConfig] { v =>
      v.exposureTime.toDuration
    }

  given Encoder[SmartGcalValue.Legacy] =
    SmartGcalTable.valueEncoder

  def encoder(using v: Encoder[SmartGcalValue.Legacy]): Encoder[TableRow] =
    (
      int8_pos *:
      v
    ).contramap[TableRow] { r => (
      r.line ,
      r.value
    )}

  private val allCols: NonEmptyList[Col] =
    NonEmptyList.of(
      Col("c_step_order", "int8"),
    ) ::: SmartGcalTable.Gcal.cols ::: NonEmptyList.one(
      Col("c_exposure_time", "interval")
    )

  private val instCols: NonEmptyList[Col] =
    NonEmptyList.of(
      Col("c_step_order", "int8"),
      Col("c_exposure_time", "interval")
    )

  // We can't use forInstrument as igrins2 has no search columns
  val tmpS: SmartGcalTable.Temp = SmartGcalTable.Temp(Igrins2, allCols)
  val instS: SmartGcalTable.Inst = SmartGcalTable.Inst(Igrins2, instCols)

  object Ig2 extends SmartGcalLoader(
    tmpS,
    instS,
    pipe    = filename => FileReader.igrins2[IO](filename) andThen FileEntry.tableRows[IO],
    encoder = encoder
  )
