// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.parse.Parser
import cats.parse.Rfc5234.digit
import lucuma.core.enums.GcalArc
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
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig

trait GnirsParsers:
  import common.*
  import util.*

  // Spectroscopy rows produce calibrations; imaging rows are skipped (Obsolete).
  private val mode: Parser[Boolean] =
    oneOf(
      "Spectroscopy" -> true,
      "Imaging"      -> false
    ).withContext("GNIRS mode")

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

  // Long slit calibrations only use slit FPUs.  Spectroscopy rows for non-slit
  // FPUs (e.g. pinholes) are skipped, marking the whole row obsolete.
  val fpu: Parser[Availability[NonEmptyList[GnirsFpu]]] =
    manyOfObsoletable(
      Set("pinhole 0.1", "pinhole 0.3"),
      "0.10 arcsec"  -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_0_10),
      "0.15 arcsec"  -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_0_15),
      "0.20 arcsec"  -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_0_20),
      "0.30 arcsec"  -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_0_30),
      "0.45 arcsec"  -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_0_45),
      "0.675 arcsec" -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_0_675),
      "1.0 arcsec"   -> GnirsFpu.Slit(GnirsFpuSlit.LongSlit_1_00)
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

  // Calibration value columns, in the file's physical order:
  // lamp, shutter, filter, diffuser, observe(count), exposure time, coadds, basecal.
  private val legacyValue: Parser[SmartGcalValue.Legacy] =
    (
      (lamp               <* columnSep) ~
      (gcal.shutter       <* columnSep) ~
      (gcal.filter        <* columnSep) ~
      (gcal.diffuser      <* columnSep) ~
      (posInt             <* columnSep) ~
      (posSecondsTimeSpan <* columnSep) ~
      (posInt             <* columnSep) ~
      gcal.baselineType
    ).map { case (((((((l, shutter), filter), diffuser), count), time), coadds), baseline) =>
      SmartGcalValue(
        Gcal(l, filter, diffuser, shutter),
        baseline,
        count,
        LegacyInstrumentConfig(time, coadds)
      )
    }

  // The search-key columns, in the file's physical order, paired with whether
  // the row is a spectroscopy row.  Non-slit (obsolete) FPUs make the whole row
  // obsolete.
  private val fileKey: Parser[Availability[(Boolean, FileKey)]] =
    (
      (mode            <* columnSep) ~
      (pixelScale      <* columnSep) ~
      (disperser       <* columnSep) ~
      (crossDispersed  <* columnSep) ~
      (wavelengthRange <* columnSep) ~
      (fpu             <* columnSep) ~
      wellDepth
    ).map { case ((((((spec, ps), disp), xd), wr), usAvail), wd) =>
      usAvail.map(us => (spec, FileKey(ps, disp, xd, wr, us, wd)))
    }

  val fileEntry: Parser[Availability[FileEntry]] =
    ((fileKey <* columnSep) ~ legacyValue).map { case (keyAvail, value) =>
      keyAvail.flatMap { case (spec, key) =>
        if spec then Availability.Current(FileEntry(key, value)) else Availability.Obsolete
      }
    }

object gnirs extends GnirsParsers
