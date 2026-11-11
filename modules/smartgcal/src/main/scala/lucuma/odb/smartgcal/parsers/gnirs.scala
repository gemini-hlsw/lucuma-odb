// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234.digit
import lucuma.core.enums.GcalArc
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GnirsFpuIfu
import lucuma.core.enums.GnirsFpuOther
import lucuma.core.enums.GnirsFpuSlit
import lucuma.core.enums.GnirsGrating
import lucuma.core.enums.GnirsPixelScale
import lucuma.core.enums.GnirsPrism
import lucuma.core.enums.GnirsWellDepth
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.parser.MiscParsers.dash
import lucuma.core.parser.MiscParsers.maybeWhiteSpace
import lucuma.core.parser.MiscParsers.posInt
import lucuma.core.util.parser.UtilParsers.posSecondsTimeSpan
import lucuma.odb.smartgcal.data.Gnirs.FileEntry
import lucuma.odb.smartgcal.data.Gnirs.FileKey
import lucuma.odb.smartgcal.data.Gnirs.FileMode
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig

trait GnirsParsers:
  import common.*
  import util.*

  val pixelScale: Parser[NonEmptyList[GnirsPixelScale]] =
    // The pixel scale column is quoted in the source file, e.g. "0.05""/pix".
    manyOf(
      "\"0.05\"\"/pix\"" -> GnirsPixelScale.PixelScale_0_05,
      "\"0.15\"\"/pix\"" -> GnirsPixelScale.PixelScale_0_15
    ).withContext("GNIRS pixel scale")

  val disperser: Parser[NonEmptyList[GnirsGrating]] =
    manyOf(
      "10 l/mm grating"  -> GnirsGrating.D10,
      "32 l/mm grating"  -> GnirsGrating.D32,
      "111 l/mm grating" -> GnirsGrating.D111
    ).withContext("GNIRS disperser")

  val crossDispersed: Parser[NonEmptyList[GnirsPrism]] =
    manyOf(
      "No"  -> GnirsPrism.Mirror,
      "SXD" -> GnirsPrism.Sxd,
      "LXD" -> GnirsPrism.Lxd
    ).withContext("GNIRS cross dispersion")

  // Both long-slit and pinhole FPUs produce calibrations.  The pinhole rows are
  // the daytime pinhole flats used to trace cross-dispersed spectral orders.
  val fpu: Parser[NonEmptyList[GnirsFpu]] =
    manyOf(
      "pinhole 0.1"  -> GnirsFpu.Other(GnirsFpuOther.Pinhole1),
      "pinhole 0.3"  -> GnirsFpu.Other(GnirsFpuOther.Pinhole3),
      "LR-IFU"       -> GnirsFpu.Spectroscopy.Ifu(GnirsFpuIfu.LowResolution),
      "HR-IFU"       -> GnirsFpu.Spectroscopy.Ifu(GnirsFpuIfu.HighResolution),
      "0.10 arcsec"  -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_10),
      "0.15 arcsec"  -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_15),
      "0.20 arcsec"  -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_20),
      "0.30 arcsec"  -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_30),
      "0.45 arcsec"  -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_45),
      "0.675 arcsec" -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_0_675),
      "1.0 arcsec"   -> GnirsFpu.Spectroscopy.Slit(GnirsFpuSlit.LongSlit_1_00)
    ).withContext("GNIRS focal plane unit")

  val wellDepth: Parser[NonEmptyList[GnirsWellDepth]] =
    manyOf(
      "Shallow" -> GnirsWellDepth.Shallow,
      "Deep"    -> GnirsWellDepth.Deep
    ).withContext("GNIRS well depth")

  // Central wavelength range, expressed in micrometers, e.g. "1.03 - 1.06".
  private val micrometers: Parser[Wavelength] =
    (digit.rep.string ~ (Parser.char('.') *> digit.rep.string).?).mapFilter { case (whole, frac) =>
      val bd = BigDecimal(frac.fold(whole)(f => s"$whole.$f"))
      Wavelength.fromIntPicometers((bd * BigDecimal(1000000)).toInt)
    }

  val wavelengthRange: Parser[BoundedInterval[Wavelength]] =
    ((micrometers <* maybeWhiteSpace <* dash <* maybeWhiteSpace) ~ micrometers)
      .mapFilter { case (lo, hi) => BoundedInterval.openUpper(lo, hi) }
      .withContext("GNIRS central wavelength range")

  // GNIRS arc lamps are joined with '+' (e.g. "Ar arc+Xe arc") rather than ';'.
  private val arcs: Parser[NonEmptySet[GcalArc]] =
    gcal.arc.repSep(Parser.char('+')).map(_.toNes)

  private val lamp: Parser[Gcal.Lamp] =
    arcs.eitherOr(gcal.continuum).map(Gcal.Lamp.fromEither)

  // The source file leaves the Basecal column empty for the pinhole rows (the
  // pinhole calibrations are taken during the day, mirroring the Day pinhole
  // flats), so treat an empty baseline as Day.
  private val baselineTypeOrDay: Parser0[GcalBaselineType] =
    gcal.baselineType.backtrack.orElse(Parser.pure(GcalBaselineType.Day))

  // Calibration value columns, in the file's physical order:
  // lamp, shutter, filter, diffuser, observe(count), exposure time, coadds, basecal.
  private val legacyValue: Parser[SmartGcalValue.Legacy] =
    (
      (lamp               <* columnSep) ~
      (gcal.shutter       <* columnSep) ~
      (gcal.filter        <* columnSep) ~
      (gcal.diffuser      <* columnSep) ~
      (observeCount       <* columnSep) ~
      (posSecondsTimeSpan <* columnSep) ~
      (posInt             <* columnSep) ~
      baselineTypeOrDay
    ).map { case (((((((l, shutter), filter), diffuser), count), time), coadds), baseline) =>
      SmartGcalValue(
        Gcal(l, filter, diffuser, shutter),
        baseline,
        count,
        LegacyInstrumentConfig(time, coadds)
      )
    }

  // Imaging lines leave the disperser, cross-disperser and FPU columns as
  // wildcards: the mirror is in and the acquisition FPU is the only one used.
  private val wildcard: Parser[Unit] =
    Parser.char('*').withContext("GNIRS wildcard")

  // The search-key columns after the mode, in the file's physical order.
  private val spectroscopyKey: Parser[FileKey] =
    (
      (pixelScale      <* columnSep) ~
      (disperser       <* columnSep) ~
      (crossDispersed  <* columnSep) ~
      (wavelengthRange <* columnSep) ~
      (fpu             <* columnSep) ~
      wellDepth
    ).map { case (((((ps, disp), xd), wr), us), wd) =>
      FileKey(ps, FileMode.Spectroscopy(disp, xd, us), wr, wd)
    }

  private val imagingKey: Parser[FileKey] =
    (
      (pixelScale      <* columnSep) ~
      (wildcard        <* columnSep) ~
      (wildcard        <* columnSep) ~
      (wavelengthRange <* columnSep) ~
      (wildcard        <* columnSep) ~
      wellDepth
    ).map { case (((((ps, _), _), wr), _), wd) =>
      FileKey(ps, FileMode.Imaging, wr, wd)
    }

  // The mode column selects how the rest of the key is read.
  val fileKey: Parser[FileKey] =
    (oneOf("Spectroscopy" -> spectroscopyKey, "Imaging" -> imagingKey).withContext("GNIRS mode") <* columnSep).flatMap(identity)

  val fileEntry: Parser[FileEntry] =
    ((fileKey <* columnSep) ~ legacyValue).map(FileEntry.apply)

object gnirs extends GnirsParsers
