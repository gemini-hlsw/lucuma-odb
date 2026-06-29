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
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.gnirs.GnirsAcquisitionMirrorMode
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import monocle.Focus
import monocle.Lens

object Gnirs:

  /**
   * Lookup key for a GNIRS smart gcal search, derived from a science dynamic
   * (and static) configuration.  The grating, cross-disperser (prism), single
   * central wavelength and slit are only present for spectroscopy (acquisition
   * mirror "out"); they are `None` otherwise so that no calibration mapping is
   * found.
   */
  case class SearchKey(
    pixelScale:     GnirsPixelScale,
    disperser:      Option[GnirsGrating],
    crossDispersed: Option[GnirsPrism],
    wavelength:     Option[Wavelength],
    fpu:            GnirsFpu,
    wellDepth:      GnirsWellDepth
  ):
    def format: String =
      val d = s"disperser: ${disperser.getOrElse("None")}"
      val x = s"crossDispersed: ${crossDispersed.getOrElse("None")}"
      val w = s"wavelength: ${wavelength.fold("None")(w => s"${w.nm.value.value} nm")}"
      val u = s"fpu: ${fpu.fold(_.toString, _.toString, _.toString)}"
      s"Gnirs { pixelScale: $pixelScale, $d, $x, $w, $u, wellDepth: $wellDepth }"

  object SearchKey:
    def fromConfig(static: GnirsStaticConfig, dyn: GnirsDynamicConfig): SearchKey =
      val (prism, grating, wavelength) =
        dyn.acquisitionMirror match
          case GnirsAcquisitionMirrorMode.Out(p, g, w) => (p.some, g.some, w.value.some)
          case GnirsAcquisitionMirrorMode.In           => (none, none, none)
      SearchKey(
        dyn.camera.pixelScale,
        grating,
        prism,
        wavelength,
        dyn.fpu,
        static.wellDepth
      )

  /**
   * Stored smart gcal table key.  Every loaded (spectroscopy) row has concrete
   * values for all components; the central wavelength is stored as a range that
   * the search wavelength must fall within.
   */
  case class TableKey(
    pixelScale:      GnirsPixelScale,
    disperser:       GnirsGrating,
    crossDispersed:  GnirsPrism,
    wavelengthRange: BoundedInterval[Wavelength],
    fpu:             GnirsFpu,
    wellDepth:       GnirsWellDepth
  )

  object TableKey:
    given Eq[TableKey] =
      Eq.by(k => (k.pixelScale, k.disperser, k.crossDispersed, k.wavelengthRange, k.fpu, k.wellDepth))

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

    def wavelengthRange: Lens[TableRow, BoundedInterval[Wavelength]] =
      TableRow.key.andThen(Focus[TableKey](_.wavelengthRange))

    def exposureTime: Lens[TableRow, TimeSpan] =
      TableRow.value
        .andThen(SmartGcalValue.instrumentConfig)
        .andThen(LegacyInstrumentConfig.exposureTime)

    def stepCount: Lens[TableRow, PosInt] =
      TableRow.value
        .andThen(SmartGcalValue.stepCount)

  /**
   * A single line of a GNIRS smart gcal definition file.  Each component may
   * specify multiple values (via wildcards), which fan out into multiple table
   * keys / rows.
   */
  case class FileKey(
    pixelScales:     NonEmptyList[GnirsPixelScale],
    dispersers:      NonEmptyList[GnirsGrating],
    crossDisperseds: NonEmptyList[GnirsPrism],
    wavelengthRange: BoundedInterval[Wavelength],
    fpus:            NonEmptyList[GnirsFpu],
    wellDepths:      NonEmptyList[GnirsWellDepth]
  ):
    def tableKeys: NonEmptyList[TableKey] =
      for
        p <- pixelScales
        d <- dispersers
        x <- crossDisperseds
        u <- fpus
        w <- wellDepths
      yield TableKey(p, d, x, wavelengthRange, u, w)

  case class FileEntry(
    key:   FileKey,
    value: SmartGcalValue.Legacy
  ):
    def tableRows(line: PosLong): NonEmptyList[TableRow] =
      key.tableKeys.map(tk => TableRow(line, tk, value))

  object FileEntry:
    def tableRows[F[_]]: Pipe[F, (PosLong, FileEntry), TableRow] =
      _.flatMap { case (line, fe) => Stream.emits(fe.tableRows(line).toList) }
