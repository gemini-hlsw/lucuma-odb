// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.data.NonEmptyList
import cats.syntax.option.*
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
import lucuma.core.model.sequence.GmosGratingConfig
import lucuma.core.syntax.enumerated.*
import lucuma.core.util.Enumerated

object GmosNorth {

  case class SearchKey(
    grating:    Option[GmosGratingConfig.North],
    filter:     Option[GmosNorthFilter],
    fpu:        Option[GmosNorthFpu],
    xBin:       GmosXBinning,
    yBin:       GmosYBinning,
    gain:       GmosAmpGain
  )

  case class GratingConfigKey(
    grating:         GmosNorthGrating,
    order:           GmosGratingOrder,
    wavelengthRange: BoundedInterval[Wavelength]
  )

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
//
//    def format: String = {
//      def quote[A: Enumerated](a: A): String =
//        s"'${a.tag}'"
//
//      def quoteOpt[A: Enumerated](a: Option[A]): String =
//        a.fold("NULL")(quote)
//
//      s"${quoteOpt(grating)}, ${quoteOpt(filter)}, ${quoteOpt(fpu)}, ${quote(xBin)}, ${quote(yBin)}, $wavelengthRange, ${quote(order)}, ${quote(gain)}"
//    }

  case class TableRow(
    line:  PosLong,
    key:   TableKey,
    value: SmartGcalValue.Legacy
  )

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


