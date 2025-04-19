// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import monocle.Focus
import monocle.Lens
import monocle.Optional

object Gmos {

  object SearchKey {

    case class North(
      grating:    Option[GmosGratingConfig.North],
      filter:     Option[GmosNorthFilter],
      fpu:        Option[GmosNorthFpu],
      xBin:       GmosXBinning,
      yBin:       GmosYBinning,
      gain:       GmosAmpGain
    ) {

      def format: String = {
        val g = s"grating: ${grating.fold("None"){g => s"(${g.grating}, ${g.order}, ${g.wavelength.nm.value.value} nm)"}}"
        val f = s"filter: ${filter.getOrElse("None")}"
        val u = s"fpu: ${fpu.getOrElse("None")}"
        s"GmosNorth { $g, $f, $u, binning: ${xBin.count}x${yBin.count}, gain: $gain }"
      }

    }

    object North {

      given Eq[North] =
        Eq.by { k => (
          k.grating,
          k.filter,
          k.fpu,
          k.xBin,
          k.yBin,
          k.gain
        )}

      def fromDynamicConfig(gn: DynamicConfig.GmosNorth): North =
        North(
          gn.gratingConfig,
          gn.filter,
          gn.fpu.flatMap(GmosFpuMask.builtin.getOption),
          gn.readout.xBin,
          gn.readout.yBin,
          gn.readout.ampGain
        )

    }

    case class South(
      grating:    Option[GmosGratingConfig.South],
      filter:     Option[GmosSouthFilter],
      fpu:        Option[GmosSouthFpu],
      xBin:       GmosXBinning,
      yBin:       GmosYBinning,
      gain:       GmosAmpGain
    ) {

      def format: String = {
        val g = s"grating: ${grating.fold("None"){g => s"(${g.grating}, ${g.order}, ${g.wavelength.nm.value.value} nm)"}}"
        val f = s"filter: ${filter.getOrElse("None")}"
        val u = s"fpu: ${fpu.getOrElse("None")}"
        s"GmosSouth { $g, $f, $u, binning: ${xBin.count}x${yBin.count}, gain: $gain }"
      }

    }

    object South {

      given Eq[South] =
        Eq.by { k => (
          k.grating,
          k.filter,
          k.fpu,
          k.xBin,
          k.yBin,
          k.gain
        )}

      def fromDynamicConfig(gs: DynamicConfig.GmosSouth): South =
        South(
          gs.gratingConfig,
          gs.filter,
          gs.fpu.flatMap(GmosFpuMask.builtin.getOption),
          gs.readout.xBin,
          gs.readout.yBin,
          gs.readout.ampGain
        )

    }

  }


  case class GratingConfigKey[G](
    grating:         G,
    order:           GmosGratingOrder,
    wavelengthRange: BoundedInterval[Wavelength]
  )

  object GratingConfigKey {

    def grating[G]: Lens[GratingConfigKey[G], G] =
      Focus[GratingConfigKey[G]](_.grating)

    def order[G]: Lens[GratingConfigKey[G], GmosGratingOrder] =
      Focus[GratingConfigKey[G]](_.order)

    def wavelengthRange[G]: Lens[GratingConfigKey[G], BoundedInterval[Wavelength]] =
      Focus[GratingConfigKey[G]](_.wavelengthRange)

  }

  case class TableKey[G, L, U](
    gratingConfig: Option[GratingConfigKey[G]],
    filter:        Option[L],
    fpu:           Option[U],
    xBin:          GmosXBinning,
    yBin:          GmosYBinning,
    gain:          GmosAmpGain
  ) {

    def grating: Option[G] =
      gratingConfig.map(_.grating)

    def order: Option[GmosGratingOrder] =
      gratingConfig.map(_.order)

    def wavelengthRange: Option[BoundedInterval[Wavelength]] =
      gratingConfig.map(_.wavelengthRange)
  }

  object TableKey {

    type North = TableKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
    type South = TableKey[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

    def gratingConfig[G, L, U]: Lens[TableKey[G, L, U], Option[GratingConfigKey[G]]] =
      Focus[TableKey[G, L, U]](_.gratingConfig)

    def filter[G, L, U]: Lens[TableKey[G, L, U], Option[L]] =
      Focus[TableKey[G, L, U]](_.filter)

    def fpu[G, L, U]: Lens[TableKey[G, L, U], Option[U]] =
      Focus[TableKey[G, L, U]](_.fpu)

    def xBin[G, L, U]: Lens[TableKey[G, L, U], GmosXBinning] =
      Focus[TableKey[G, L, U]](_.xBin)

    def yBin[G, L, U]: Lens[TableKey[G, L, U], GmosYBinning] =
      Focus[TableKey[G, L, U]](_.yBin)

    def gain[G, L, U]: Lens[TableKey[G, L, U], GmosAmpGain] =
      Focus[TableKey[G, L, U]](_.gain)

  }

  case class TableRow[G, L, U](
    line:  PosLong,
    key:   TableKey[G, L, U],
    value: SmartGcalValue.Legacy
  )

  object TableRow {

    type North = TableRow[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
    type South = TableRow[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

    def line[G, L, U]: Lens[TableRow[G, L, U], PosLong] =
      Focus[TableRow[G, L, U]](_.line)

    def key[G, L, U]: Lens[TableRow[G, L, U], TableKey[G, L, U]] =
      Focus[TableRow[G, L, U]](_.key)

    def value[G, L, U]: Lens[TableRow[G, L, U], SmartGcalValue.Legacy] =
      Focus[TableRow[G, L, U]](_.value)

    def grating[G, L, U]: Optional[TableRow[G, L, U], G] =
      TableRow.key
        .andThen(TableKey.gratingConfig)
        .andThen(monocle.std.option.some)
        .andThen(GratingConfigKey.grating)

    def wavelengthRange[G, L, U]: Optional[TableRow[G, L, U], BoundedInterval[Wavelength]] =
      TableRow.key
        .andThen(TableKey.gratingConfig)
        .andThen(monocle.std.option.some)
        .andThen(GratingConfigKey.wavelengthRange)

    def exposureTime[G, L, U]: Lens[TableRow[G, L, U], TimeSpan] =
      TableRow.value
        .andThen(SmartGcalValue.instrumentConfig)
        .andThen(LegacyInstrumentConfig.exposureTime)

    def stepCount[G, L, U]: Lens[TableRow[G, L, U], PosInt] =
      TableRow.value
        .andThen(SmartGcalValue.stepCount)

  }

  case class FileKey[G, L, U](
    gratings:        NonEmptyList[Option[G]],
    filters:         NonEmptyList[Option[L]],
    fpus:            NonEmptyList[Option[U]],
    xBin:            GmosXBinning,
    yBin:            GmosYBinning,
    wavelengthRange: BoundedInterval[Wavelength],
    orders:          NonEmptyList[GmosGratingOrder],
    gains:           NonEmptyList[GmosAmpGain]
  ) {

    def tableKeys: NonEmptyList[TableKey[G, L, U]] =
      for {
        g <- gratings
        c <- g.fold(NonEmptyList.one(none[GratingConfigKey[G]])) { g => orders.map(o => GratingConfigKey(g, o, wavelengthRange).some) }
        f <- filters
        u <- fpus
        n <- gains
      } yield TableKey(c, f, u, xBin, yBin, n)

  }

  object FileKey {

    type North = FileKey[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
    type South = FileKey[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

  }

  case class FileEntry[G, L, U](
    key:   FileKey[G, L, U],
    value: SmartGcalValue.Legacy
  ) {

    def tableRows(line: PosLong): NonEmptyList[TableRow[G, L, U]] =
      key.tableKeys.map { tk => TableRow(line, tk, value) }

  }

  object FileEntry {

    type North = FileEntry[GmosNorthGrating, GmosNorthFilter, GmosNorthFpu]
    type South = FileEntry[GmosSouthGrating, GmosSouthFilter, GmosSouthFpu]

    def tableRows[F[_], G, L, U]: Pipe[F, (PosLong, FileEntry[G, L, U]), TableRow[G, L, U]] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }

    def tableRowsNorth[F[_]]: Pipe[F, (PosLong, North), TableRow.North] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }

    def tableRowsSouth[F[_]]: Pipe[F, (PosLong, South), TableRow.South] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }
  }

}


