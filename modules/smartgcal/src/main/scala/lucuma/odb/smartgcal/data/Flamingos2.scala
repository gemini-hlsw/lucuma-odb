// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.model.sequence.f2.F2DynamicConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import monocle.Focus
import monocle.Lens
import monocle.Optional

object Flamingos2:

  case class TableKey(
    disperser: Option[F2Disperser],
    filter:    F2Filter,
    fpu:       Option[F2Fpu]
  )

  object TableKey:
    def fromDynamicConfig(f2: F2DynamicConfig): TableKey =
      TableKey(
        f2.disperser,
        f2.filter,
        f2.fpu.builtin.map(_.value)
      )

    def disperser: Lens[TableKey, Option[F2Disperser]] =
      Focus[TableKey](_.disperser)

    def filter: Lens[TableKey, F2Filter] =
      Focus[TableKey](_.filter)

    def fpu: Lens[TableKey, Option[F2Fpu]] =
      Focus[TableKey](_.fpu)

  case class TableRow(
    line:  PosLong,
    key:   TableKey,
    value: SmartGcalValue.Legacy
  )

  object TableRow:

    def line: Lens[TableRow, PosLong] =
      Focus[TableRow](_.line)

    def key: Lens[TableRow, TableKey] =
      Focus[TableRow](_.key)

    def value: Lens[TableRow, SmartGcalValue.Legacy] =
      Focus[TableRow](_.value)

    def disperser: Optional[TableRow, F2Disperser] =
      TableRow.key
        .andThen(TableKey.disperser)
        .andThen(monocle.std.option.some)

    def exposureTime: Lens[TableRow, TimeSpan] =
      TableRow.value
        .andThen(SmartGcalValue.instrumentConfig)
        .andThen(LegacyInstrumentConfig.exposureTime)

    def stepCount: Lens[TableRow, PosInt] =
      TableRow.value
        .andThen(SmartGcalValue.stepCount)

  case class FileKey(
    dispersers:      NonEmptyList[Option[F2Disperser]],
    filters:         NonEmptyList[F2Filter],
    fpus:            NonEmptyList[F2Fpu]
  ) {

    def tableKeys: NonEmptyList[TableKey] =
      for {
        d <- dispersers
        c <- d.fold(NonEmptyList.one(none)) { g => NonEmptyList.one(g.some) }
        f <- filters
        u <- fpus
      } yield TableKey(c, f, u.some)

  }

  case class FileEntry(
    key:   FileKey,
    value: SmartGcalValue.Legacy
  ) {

    def tableRows(line: PosLong): NonEmptyList[TableRow] =
      key.tableKeys.map { tk => TableRow(line, tk, value) }

  }

  object FileEntry:

    def tableRows[F[_]]: Pipe[F, (PosLong, FileEntry), TableRow] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }

