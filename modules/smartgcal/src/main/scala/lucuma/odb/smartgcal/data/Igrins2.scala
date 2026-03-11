// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.data.NonEmptyList
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import fs2.Stream
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import monocle.Focus
import monocle.Lens

object Igrins2:

  object TableKey:
    def format: String =
      "Igrins2"

  case class TableRow(
    line:  PosLong,
    key:   TableKey.type,
    value: SmartGcalValue.Legacy
  )

  object TableRow:

    def line: Lens[TableRow, PosLong] =
      Focus[TableRow](_.line)

    def key: Lens[TableRow, TableKey.type] =
      Focus[TableRow](_.key)

    def value: Lens[TableRow, SmartGcalValue.Legacy] =
      Focus[TableRow](_.value)

    def exposureTime: Lens[TableRow, TimeSpan] =
      TableRow.value
        .andThen(SmartGcalValue.instrumentConfig)
        .andThen(LegacyInstrumentConfig.exposureTime)

    def stepCount: Lens[TableRow, PosInt] =
      TableRow.value
        .andThen(SmartGcalValue.stepCount)

  case class FileKey():
    def tableKeys: NonEmptyList[TableKey.type] =
      NonEmptyList.one(TableKey)

  case class FileEntry(
    key:   FileKey,
    value: SmartGcalValue.Legacy
  ):
    def tableRows(line: PosLong): NonEmptyList[TableRow] =
      key.tableKeys.map(tk => TableRow(line, tk, value))

  object FileEntry:

    def tableRows[F[_]]: Pipe[F, (PosLong, FileEntry), TableRow] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }
