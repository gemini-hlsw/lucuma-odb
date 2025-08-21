// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.option.*
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.odb.smartgcal.data.Gmos.FileEntry
import lucuma.odb.smartgcal.data.Gmos.FileKey

trait GmosSouthParsers extends GmosCommonParsers {
  import common.*
  import util.*

  val filter: Parser[NonEmptyList[Option[GmosSouthFilter]]] =
    Parser.string("none").as(NonEmptyList.one(none[GmosSouthFilter])) |   // "none" found in existing .csv files
      manyOfOption("None",
        "u_G0332"                   -> GmosSouthFilter.UPrime,
        "g_G0325"                   -> GmosSouthFilter.GPrime,
        "r_G0326"                   -> GmosSouthFilter.RPrime,
        "i_G0327"                   -> GmosSouthFilter.IPrime,
        "z_G0328"                   -> GmosSouthFilter.ZPrime,
        "Z_G0343"                   -> GmosSouthFilter.Z,
        "Y_G0344"                   -> GmosSouthFilter.Y,
        "GG455_G0329"               -> GmosSouthFilter.GG455,
        "OG515_G0330"               -> GmosSouthFilter.OG515,
        "RG610_G0331"               -> GmosSouthFilter.RG610,
        "RG780_G0334"               -> GmosSouthFilter.RG780,
        "CaT_G0333"                 -> GmosSouthFilter.CaT,
        "HartmannA_G0337 + r_G0326" -> GmosSouthFilter.HartmannA_RPrime,
        "HartmannB_G0338 + r_G0326" -> GmosSouthFilter.HartmannB_RPrime,
        "g_G0325 + GG455_G0329"     -> GmosSouthFilter.GPrime_GG455,
        "g_G0325 + OG515_G0330"     -> GmosSouthFilter.GPrime_OG515,
        "r_G0326 + RG610_G0331"     -> GmosSouthFilter.RPrime_RG610,
        "i_G0327 + RG780_G0334"     -> GmosSouthFilter.IPrime_RG780,
        "i_G0327 + CaT_G0333"       -> GmosSouthFilter.IPrime_CaT,
        "z_G0328 + CaT_G0333"       -> GmosSouthFilter.ZPrime_CaT,
        "Ha_G0336"                  -> GmosSouthFilter.Ha,
        "SII_G0335"                 -> GmosSouthFilter.SII,
        "HaC_G0337"                 -> GmosSouthFilter.HaC,
        "OIII_G0338"                -> GmosSouthFilter.OIII,
        "OIIIC_G0339"               -> GmosSouthFilter.OIIIC,
        "HeII_G0340"                -> GmosSouthFilter.HeII,
        "HeIIC_G0341"               -> GmosSouthFilter.HeIIC,
        "Lya395_G0342"              -> GmosSouthFilter.HeII // Lya395 was removed
      ).withContext("GMOS South filter")

  val fpu: Parser[NonEmptyList[Option[GmosSouthFpu]]] =
    manyOfOption("None",
      "N and S 0.50 arcsec"  -> GmosSouthFpu.Ns1,
      "N and S 0.75 arcsec"  -> GmosSouthFpu.Ns2,
      "N and S 1.00 arcsec"  -> GmosSouthFpu.Ns3,
      "N and S 1.50 arcsec"  -> GmosSouthFpu.Ns4,
      "N and S 2.00 arcsec"  -> GmosSouthFpu.Ns5,
      "Longslit 0.25 arcsec" -> GmosSouthFpu.LongSlit_0_25,
      "Longslit 0.50 arcsec" -> GmosSouthFpu.LongSlit_0_50,
      "Longslit 0.75 arcsec" -> GmosSouthFpu.LongSlit_0_75,
      "Longslit 1.00 arcsec" -> GmosSouthFpu.LongSlit_1_00,
      "Longslit 1.50 arcsec" -> GmosSouthFpu.LongSlit_1_50,
      "Longslit 2.00 arcsec" -> GmosSouthFpu.LongSlit_2_00,
      "Longslit 5.00 arcsec" -> GmosSouthFpu.LongSlit_5_00,
      "IFU 2 Slits"          -> GmosSouthFpu.Ifu2Slits,
      "IFU Left Slit (blue)" -> GmosSouthFpu.IfuBlue,
      "IFU Right Slit (red)" -> GmosSouthFpu.IfuRed,
      "IFU N and S 2 Slits"          -> GmosSouthFpu.IfuNS2Slits,
      "IFU N and S Left Slit (blue)" -> GmosSouthFpu.IfuNSBlue,
      "IFU N and S Right Slit (red)" -> GmosSouthFpu.IfuNSRed
    ).withContext("GMOS South FPU")

  val grating: Parser[Availability[NonEmptyList[Option[GmosSouthGrating]]]] =
    manyOfObsoletableOptionEnumerated[GmosSouthGrating]("Mirror", Set("B600_G5323")).withContext("GMOS South grating")

  val fileKey: Parser[Availability[FileKey.South]] =
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
      grating.map(g => FileKey(g, filter, fpu, xBin, yBin, range, order, gain))
    }

  def fileEntry: Parser[Availability[FileEntry.South]] =
    ((fileKey <* file.keyValueSep) ~ file.legacyValue).map { case (k, v) =>
      k.map(k => FileEntry(k, v))
    }

}

object gmosSouth extends GmosSouthParsers
