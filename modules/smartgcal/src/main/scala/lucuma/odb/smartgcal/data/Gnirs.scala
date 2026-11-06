// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.data

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import fs2.Pipe
import fs2.Stream
import lucuma.core.enums.GnirsFpuOther
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
   * (and static) configuration.  The grating and cross-disperser (prism) are
   * only present for spectroscopy (acquisition mirror "out"), where the
   * wavelength is the grating's; imaging (mirror "in") is keyed by the filter's
   * central wavelength instead.
   */
  case class SearchKey(
    pixelScale:     GnirsPixelScale,
    disperser:      Option[GnirsGrating],
    crossDispersed: Option[GnirsPrism],
    wavelength:     Wavelength,
    fpu:            GnirsFpu,
    wellDepth:      GnirsWellDepth
  ):
    def format: String =
      val d = s"disperser: ${disperser.getOrElse("None")}"
      val x = s"crossDispersed: ${crossDispersed.getOrElse("None")}"
      val w = s"wavelength: ${wavelength.nm.value.value} nm"
      val u = s"fpu: ${fpu.fold(_.toString, _.toString, _.toString)}"
      s"Gnirs { pixelScale: $pixelScale, $d, $x, $w, $u, wellDepth: $wellDepth }"

  object SearchKey:
    def fromConfig(static: GnirsStaticConfig, dyn: GnirsDynamicConfig): SearchKey =
      val (prism: Option[GnirsPrism], grating: Option[GnirsGrating], wavelength: Wavelength) =
        dyn.acquisitionMirror match
          case GnirsAcquisitionMirrorMode.Out(p, g, w) => (p.some, g.some, w.value)
          case GnirsAcquisitionMirrorMode.In           => (none, none, dyn.filter.centralWavelength)
      SearchKey(
        dyn.camera.pixelScale,
        grating,
        prism,
        wavelength,
        dyn.fpu,
        static.wellDepth
      )

  /**
   * Stored smart gcal table key.  Spectroscopy rows name the grating and
   * cross-disperser; imaging rows have neither (acquisition mirror "in") and
   * image through the acquisition FPU.  The central wavelength is stored as a
   * range that the search wavelength must fall within.
   */
  case class TableKey(
    pixelScale:      GnirsPixelScale,
    disperser:       Option[GnirsGrating],
    crossDispersed:  Option[GnirsPrism],
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
   * The mode-specific columns of a definition file line. Spectroscopy lines
   * name the grating, cross-disperser and FPU (each possibly a wildcard);
   * imaging lines leave all three as wildcards, since imaging always goes
   * through the acquisition FPU with the mirror in.
   */
  enum FileMode:
    case Imaging
    case Spectroscopy(
      dispersers:      NonEmptyList[GnirsGrating],
      crossDisperseds: NonEmptyList[GnirsPrism],
      fpus:            NonEmptyList[GnirsFpu]
    )

  /**
   * A single line of a GNIRS smart gcal definition file.  Each component may
   * specify multiple values (via wildcards), which fan out into multiple table
   * keys / rows.
   */
  case class FileKey(
    pixelScales:     NonEmptyList[GnirsPixelScale],
    mode:            FileMode,
    wavelengthRange: BoundedInterval[Wavelength],
    wellDepths:      NonEmptyList[GnirsWellDepth]
  ):
    def tableKeys: NonEmptyList[TableKey] =
      val modes: NonEmptyList[(Option[GnirsGrating], Option[GnirsPrism], GnirsFpu)] =
        mode match
          case FileMode.Imaging                                                      =>
            NonEmptyList.one((none, none, GnirsFpu.Other(GnirsFpuOther.Acquisition)))
          case FileMode.Spectroscopy(dispersers = ds, crossDisperseds = xs, fpus = us) =>
            for
              d <- ds
              x <- xs
              u <- us
            yield (d.some, x.some, u)
      for
        p         <- pixelScales
        (d, x, u) <- modes
        w         <- wellDepths
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
