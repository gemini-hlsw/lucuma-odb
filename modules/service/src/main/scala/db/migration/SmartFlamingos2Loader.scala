// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

import cats.data.NonEmptyList
import cats.effect.IO
import lucuma.core.enums.Instrument.Flamingos2
import lucuma.odb.smartgcal.FileReader
import lucuma.odb.smartgcal.data.Flamingos2.FileEntry
import lucuma.odb.smartgcal.data.Flamingos2.TableKey
import lucuma.odb.smartgcal.data.Flamingos2.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.util.Codecs.*
import lucuma.odb.util.Flamingos2Codecs.*
import skunk.Encoder
import skunk.codec.temporal.interval

object SmartFlamingos2Loader {

  import SmartGcalTable.Col

  given Encoder[SmartGcalValue.LegacyInstrumentConfig] =
    interval.contramap[SmartGcalValue.LegacyInstrumentConfig] { v =>
      v.exposureTime.toDuration
    }

  given Encoder[SmartGcalValue.Legacy] =
    SmartGcalTable.valueEncoder

  given Encoder[TableKey] =
    (
      flamingos_2_disperser.opt *:
      flamingos_2_filter        *:
      flamingos_2_fpu.opt
    ).contramap[TableKey] { k => (
      k.disperser,
      k.filter   ,
      k.fpu      ,
    )}

  private def keyColumns: NonEmptyList[Col] =
    NonEmptyList.of(
      Col.fkey("c_disperser", s"t_f2_disperser").index,
      Col.fkey("c_filter", s"t_f2_filter").index,
      Col.fkey("c_fpu", s"t_f2_fpu").index,
    )

  def encoder(using k: Encoder[TableKey], v: Encoder[SmartGcalValue.Legacy]): Encoder[TableRow] =
    (
      int8_pos *:
      k        *:
      v
    ).contramap[TableRow] { r => (
      r.line ,
      r.key  ,
      r.value
    )}

  val (tmpS, instS) = SmartGcalTable.forInstrument(
    Flamingos2,
    Col("c_step_order", "int8"),
    keyColumns,
    NonEmptyList.one(Col("c_exposure_time", "interval"))
  )

  object F2 extends SmartGcalLoader(
    tmpS,
    instS,
    pipe    = filename => FileReader.flamingos2[IO](filename) andThen FileEntry.tableRows[IO],
    encoder = encoder
  )

}
