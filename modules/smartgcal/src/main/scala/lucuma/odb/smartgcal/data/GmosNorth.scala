// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.DynamicConfig
import lucuma.core.model.sequence.GmosFpuMask
import lucuma.core.model.sequence.GmosGratingConfig
import lucuma.core.syntax.enumerated.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import monocle.Focus
import monocle.Lens
import monocle.Optional

object GmosNorth {

  case class SearchKey(
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

  object SearchKey {

    def fromDynamicConfig(gn: DynamicConfig.GmosNorth): SearchKey =
      SearchKey(
        gn.gratingConfig,
        gn.filter,
        gn.fpu.flatMap(GmosFpuMask.builtin.getOption),
        gn.readout.xBin,
        gn.readout.yBin,
        gn.readout.ampGain
      )

    given Eq[SearchKey] =
      Eq.by { k => (
        k.grating,
        k.filter,
        k.fpu,
        k.xBin,
        k.yBin,
        k.gain
      )}

  }

  case class GratingConfigKey(
    grating:         GmosNorthGrating,
    order:           GmosGratingOrder,
    wavelengthRange: BoundedInterval[Wavelength]
  )

  object GratingConfigKey {

    val grating: Lens[GratingConfigKey, GmosNorthGrating] =
      Focus[GratingConfigKey](_.grating)

    val order: Lens[GratingConfigKey, GmosGratingOrder] =
      Focus[GratingConfigKey](_.order)

    val wavelengthRange: Lens[GratingConfigKey, BoundedInterval[Wavelength]] =
      Focus[GratingConfigKey](_.wavelengthRange)

  }

  case class TableKey(
    gratingConfig: Option[GratingConfigKey],
    filter:        Option[GmosNorthFilter],
    fpu:           Option[GmosNorthFpu],
    xBin:          GmosXBinning,
    yBin:          GmosYBinning,
    gain:          GmosAmpGain
  ) {

    def grating: Option[GmosNorthGrating] =
      gratingConfig.map(_.grating)

    def order: Option[GmosGratingOrder] =
      gratingConfig.map(_.order)

    def wavelengthRange: Option[BoundedInterval[Wavelength]] =
      gratingConfig.map(_.wavelengthRange)
  }

  object TableKey {

    val gratingConfig: Lens[TableKey, Option[GratingConfigKey]] =
      Focus[TableKey](_.gratingConfig)

    val filter: Lens[TableKey, Option[GmosNorthFilter]] =
      Focus[TableKey](_.filter)

    val fpu: Lens[TableKey, Option[GmosNorthFpu]] =
      Focus[TableKey](_.fpu)

    val xBin: Lens[TableKey, GmosXBinning] =
      Focus[TableKey](_.xBin)

    val yBin: Lens[TableKey, GmosYBinning] =
      Focus[TableKey](_.yBin)

    val gain: Lens[TableKey, GmosAmpGain] =
      Focus[TableKey](_.gain)

  }

  case class TableRow(
    line:  PosLong,
    key:   TableKey,
    value: SmartGcalValue.Legacy
  )

  object TableRow {

    val line: Lens[TableRow, PosLong] =
      Focus[TableRow](_.line)

    val key: Lens[TableRow, TableKey] =
      Focus[TableRow](_.key)

    val value: Lens[TableRow, SmartGcalValue.Legacy] =
      Focus[TableRow](_.value)

    val grating: Optional[TableRow, GmosNorthGrating] =
      TableRow.key
        .andThen(TableKey.gratingConfig)
        .andThen(monocle.std.option.some)
        .andThen(GratingConfigKey.grating)

    val wavelengthRange: Optional[TableRow, BoundedInterval[Wavelength]] =
      TableRow.key
        .andThen(TableKey.gratingConfig)
        .andThen(monocle.std.option.some)
        .andThen(GratingConfigKey.wavelengthRange)

    val exposureTime: Lens[TableRow, TimeSpan] =
      TableRow.value
        .andThen(SmartGcalValue.instrumentConfig)
        .andThen(LegacyInstrumentConfig.exposureTime)

    val stepCount: Lens[TableRow, PosInt] =
      TableRow.value
        .andThen(SmartGcalValue.stepCount)

  }

  case class FileKey(
    gratings:        NonEmptyList[Option[GmosNorthGrating]],
    filters:         NonEmptyList[Option[GmosNorthFilter]],
    fpus:            NonEmptyList[Option[GmosNorthFpu]],
    xBin:            GmosXBinning,
    yBin:            GmosYBinning,
    wavelengthRange: BoundedInterval[Wavelength],
    orders:          NonEmptyList[GmosGratingOrder],
    gains:           NonEmptyList[GmosAmpGain]
  ) {

    def tableKeys: NonEmptyList[TableKey] =
      for {
        g <- gratings
        c <- g.fold(NonEmptyList.one(none[GratingConfigKey])) { g => orders.map(o => GratingConfigKey(g, o, wavelengthRange).some) }
        f <- filters
        u <- fpus
        n <- gains
      } yield TableKey(c, f, u, xBin, yBin, n)

  }

  case class FileEntry(
    key:   FileKey,
    value: SmartGcalValue.Legacy
  ) {

    def tableRows(line: PosLong): NonEmptyList[TableRow] =
      key.tableKeys.map { tk => TableRow(line, tk, value) }

  }

  object FileEntry {

    def tableRows[F[_]]: Pipe[F, (PosLong, FileEntry), TableRow] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }

  }

}


