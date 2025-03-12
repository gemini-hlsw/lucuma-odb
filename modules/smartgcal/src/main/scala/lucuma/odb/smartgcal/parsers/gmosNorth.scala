// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.all.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.odb.smartgcal.data.Gmos.FileEntry
import lucuma.odb.smartgcal.data.Gmos.FileKey

trait GmosNorthParsers extends GmosCommonParsers:
  import common.*
  import util.*

  val filter: Parser[Availability[NonEmptyList[Option[GmosNorthFilter]]]] =
    Parser.string("none").as(NonEmptyList.one(none[GmosNorthFilter]).pure[Availability]) | // "none" found in existing .csv files
      manyOfObsoletableOption("None", Set("u_G0309"),
        "g_G0301"                   -> GmosNorthFilter.GPrime,
        "r_G0303"                   -> GmosNorthFilter.RPrime,
        "i_G0302"                   -> GmosNorthFilter.IPrime,
        "z_G0304"                   -> GmosNorthFilter.ZPrime,
        "Z_G0322"                   -> GmosNorthFilter.Z,
        "Y_G0323"                   -> GmosNorthFilter.Y,
        "ri_G0349"                  -> GmosNorthFilter.Ri,
        "GG455_G0305"               -> GmosNorthFilter.GG455,
        "OG515_G0306"               -> GmosNorthFilter.OG515,
        "RG610_G0307"               -> GmosNorthFilter.RG610,
        "CaT_G0309"                 -> GmosNorthFilter.CaT,
        "Ha_G0310"                  -> GmosNorthFilter.Ha,
        "HaC_G0311"                 -> GmosNorthFilter.HaC,
        "DS920_G0312"               -> GmosNorthFilter.DS920,
        "SII_G0317"                 -> GmosNorthFilter.SII,
        "OIII_G0318"                -> GmosNorthFilter.OIII,
        "OIIIC_G0319"               -> GmosNorthFilter.OIIIC,
        "HeII_G0320"                -> GmosNorthFilter.HeII,
        "HeIIC_G0321"               -> GmosNorthFilter.HeIIC,
        "OVI_G0345"                 -> GmosNorthFilter.OVI,
        "OVIC_G0346"                -> GmosNorthFilter.OVIC,
        "HartmannA_G0313 + r_G0303" -> GmosNorthFilter.HartmannA_RPrime,
        "HartmannB_G0314 + r_G0303" -> GmosNorthFilter.HartmannB_RPrime,
        "g_G0301 + GG455_G0305"     -> GmosNorthFilter.GPrime_GG455,
        "g_G0301 + OG515_G0306"     -> GmosNorthFilter.GPrime_OG515,
        "r_G0303 + RG610_G0307"     -> GmosNorthFilter.RPrime_RG610,
        "i_G0302 + CaT_G0309"       -> GmosNorthFilter.IPrime_CaT,
        "z_G0304 + CaT_G0309"       -> GmosNorthFilter.ZPrime_CaT
      ).withContext("GMOS North filter")

  val fpu: Parser[NonEmptyList[Option[GmosNorthFpu]]] =
    manyOfOption("None",
      "Longslit 0.25 arcsec" -> GmosNorthFpu.LongSlit_0_25,
      "Longslit 0.50 arcsec" -> GmosNorthFpu.LongSlit_0_50,
      "Longslit 0.75 arcsec" -> GmosNorthFpu.LongSlit_0_75,
      "Longslit 1.00 arcsec" -> GmosNorthFpu.LongSlit_1_00,
      "Longslit 1.50 arcsec" -> GmosNorthFpu.LongSlit_1_50,
      "Longslit 2.00 arcsec" -> GmosNorthFpu.LongSlit_2_00,
      "Longslit 5.00 arcsec" -> GmosNorthFpu.LongSlit_5_00,
      "IFU 2 Slits"          -> GmosNorthFpu.Ifu2Slits,
      "IFU Left Slit (blue)" -> GmosNorthFpu.IfuBlue,
      "IFU Right Slit (red)" -> GmosNorthFpu.IfuRed,
      "N and S 0.25 arcsec"  -> GmosNorthFpu.Ns0,
      "N and S 0.50 arcsec"  -> GmosNorthFpu.Ns1,
      "N and S 0.75 arcsec"  -> GmosNorthFpu.Ns2,
      "N and S 1.00 arcsec"  -> GmosNorthFpu.Ns3,
      "N and S 1.50 arcsec"  -> GmosNorthFpu.Ns4,
      "N and S 2.00 arcsec"  -> GmosNorthFpu.Ns5
    ).withContext("GMOS North FPU")

  val grating: Parser[Availability[NonEmptyList[Option[GmosNorthGrating]]]] =
    manyOfObsoletableOptionEnumerated[GmosNorthGrating]("Mirror", Set("B600_G5303", "B600_G5307", "R150_G5306")).withContext("GMOS North grating")

  val fileKey: Parser[Availability[FileKey.North]] =
    (
      (grating           <* columnSep) ~
      (filter            <* columnSep) ~
      (fpu               <* columnSep) ~
      (xBinning          <* columnSep) ~
      (yBinning          <* columnSep) ~
      (wavelengthRangeNm <* columnSep) ~
      (order             <* columnSep) ~
      gain
    ).map { case (((((((grating, filter), fpu), xBin), yBin), range), order), gain) =>
      (grating, filter).mapN((g, f) => FileKey(g, f, fpu, xBin, yBin, range, order, gain))
    }

  def fileEntry: Parser[Availability[FileEntry.North]] =
    ((fileKey <* file.keyValueSep) ~ file.legacyValue).map { case (k, v) =>
      k.map(k => FileEntry(k, v))
    }

object gmosNorth extends GmosNorthParsers