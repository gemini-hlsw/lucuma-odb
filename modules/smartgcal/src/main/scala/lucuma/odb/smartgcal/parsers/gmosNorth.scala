// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.odb.smartgcal.data.GmosNorth.FileKey
import lucuma.odb.smartgcal.data.GmosNorth.FileEntry

import scala.collection.immutable.ListMap

trait GmosNorthParsers {
  import common.*
  import util.*

  val filter: Parser[NonEmptyList[Option[GmosNorthFilter]]] =
    Parser.string("none").as(NonEmptyList.one(none[GmosNorthFilter])) |   // "none" found in existing .csv files
      pattern(optionMap(ListMap(
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
        "z_G0304 + CaT_G0309"       -> GmosNorthFilter.ZPrime_CaT,
        "u_G0308"                   -> GmosNorthFilter.UPrime
      ), "None")).withContext("GMOS North filter")

  val fpu: Parser[NonEmptyList[Option[GmosNorthFpu]]] =
    pattern(optionMap(ListMap(
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
    ), "None")).withContext("GMOS North FPU")

  val grating: Parser[NonEmptyList[Option[GmosNorthGrating]]] =
    pattern(optionMap(enumeratedMap[GmosNorthGrating], "Mirror")).withContext("GMOS North grating")

  val xBinning: Parser[GmosXBinning] =
    mapping(GmosXBinning.all.fproductLeft(_.count.toString)*).withContext("GMOS X-Binning")

  val yBinning: Parser[GmosYBinning] =
    mapping(GmosYBinning.all.fproductLeft(_.count.toString)*).withContext("GMOS Y-Binning")

  val order: Parser[NonEmptyList[GmosGratingOrder]] =
    mappingPattern(GmosGratingOrder.all.fproductLeft(_.count.toString)*).withContext("GMOS grating order")

  val gain: Parser[NonEmptyList[GmosAmpGain]] =
    enumeratedPattern[GmosAmpGain].withContext("GMOS amp gain")

  val fileKey: Parser[FileKey] =
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
      FileKey(grating, filter, fpu, xBin, yBin, range, order, gain)
    }

  def fileEntry: Parser[FileEntry] =
    ((fileKey <* file.keyValueSep) ~ file.legacyValue).map { case (k, v) =>
      FileEntry(k, v)
    }

}

object gmosNorth extends GmosNorthParsers
