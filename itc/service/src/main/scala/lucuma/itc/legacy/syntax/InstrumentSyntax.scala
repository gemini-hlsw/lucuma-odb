// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.legacy.syntax

import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthGrating.*
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthGrating.*

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
trait GmosSouthGratingSyntax:
  extension (self: GmosSouthGrating)
    // pedantic: tags are the same in OCS2 and OCS3 but this is just a coincidence
    def ocs2Tag: String =
      self match
        case B1200_G5321 => "B1200_G5321"
        case R831_G5322  => "R831_G5322"
        case R600_G5324  => "R600_G5324"
        case B480_G5327  => "B480_G5327"
        case R400_G5325  => "R400_G5325"
        case R150_G5326  => "R150_G5326"

object gmossouthgrating extends GmosSouthGratingSyntax

/**
 * Syntax extensions for missing properties. These need to be folded back into the lucuma.core
 * enumerations.
 */
trait GmosNorthGratingSyntax:
  extension (self: GmosNorthGrating)
    // pedantic: tags are the same in OCS2 and OCS3 but this is just a coincidence
    def ocs2Tag: String =
      self match
        case B1200_G5301 => "B1200_G5301"
        case R831_G5302  => "R831_G5302"
        case B480_G5309  => "B480_G5309"
        case R600_G5304  => "R600_G5304"
        case R400_G5310  => "R400_G5305" // NEW to OLD, for now
        case R150_G5308  => "R150_G5308"

object gmosnorthgrating extends GmosNorthGratingSyntax

trait GmosSouthFilterSyntax:
  import lucuma.core.enums.GmosSouthFilter
  import lucuma.core.enums.GmosSouthFilter.*

  extension (self: GmosSouthFilter)
    def ocs2Tag: String =
      self match
        case GPrime           => "g_G0325"
        case RPrime           => "r_G0326"
        case IPrime           => "i_G0327"
        case ZPrime           => "z_G0328"
        case Z                => "Z_G0343"
        case Y                => "Y_G0344"
        case GG455            => "GG455_G0329"
        case OG515            => "OG515_G0330"
        case RG610            => "RG610_G0331"
        case RG780            => "RG780_G0334"
        case CaT              => "CaT_G0333"
        case Ha               => "Ha_G0336"
        case HaC              => "HaC_G0337"
        case SII              => "SII_G0335"
        case OIII             => "OIII_G0338"
        case OIIIC            => "OIIIC_G0339"
        case HeII             => "HeII_G0340"
        case HeIIC            => "HeIIC_G0341"
        case OVI              => "OVI_G0347"
        case OVIC             => "OVIC_G0348"
        case HartmannA_RPrime => "HartmannA_G0337_r_G0326"
        case HartmannB_RPrime => "HartmannB_G0338_r_G0326"
        case GPrime_GG455     => "g_G0325_GG455_G0329"
        case GPrime_OG515     => "g_G0325_OG515_G0330"
        case RPrime_RG610     => "r_G0326_RG610_G0331"
        case IPrime_RG780     => "i_G0327_RG780_G0334"
        case IPrime_CaT       => "i_G0327_CaT_G0333"
        case ZPrime_CaT       => "z_G0328_CaT_G0333"
        case UPrime           => "u_G0332"

object gmossouthfilter extends GmosSouthFilterSyntax

trait GmosNorthFilterSyntax:
  import lucuma.core.enums.GmosNorthFilter
  import lucuma.core.enums.GmosNorthFilter.*
  extension (self: GmosNorthFilter)
    def ocs2Tag: String =
      self match
        case GPrime           => "g_G0301"
        case RPrime           => "r_G0303"
        case IPrime           => "i_G0302"
        case ZPrime           => "z_G0304"
        case Z                => "Z_G0322"
        case Y                => "Y_G0323"
        case Ri               => "ri_G0349"
        case GG455            => "GG455_G0305"
        case OG515            => "OG515_G0306"
        case RG610            => "RG610_G0307"
        case CaT              => "CaT_G0309"
        case Ha               => "Ha_G0310"
        case HaC              => "HaC_G0311"
        case DS920            => "DS920_G0312"
        case SII              => "SII_G0317"
        case OIII             => "OIII_G0318"
        case OIIIC            => "OIIIC_G0319"
        case HeII             => "HeII_G0320"
        case HeIIC            => "HeIIC_G0321"
        case OVI              => "OVI_G0345"
        case OVIC             => "OVIC_G0346"
        case HartmannA_RPrime => "HartmannA_G0313_r_G0303"
        case HartmannB_RPrime => "HartmannB_G0314_r_G0303"
        case GPrime_GG455     => "g_G0301_GG455_G0305"
        case GPrime_OG515     => "g_G0301_OG515_G0306"
        case RPrime_RG610     => "r_G0303_RG610_G0307"
        case IPrime_CaT       => "i_G0302_CaT_G0309"
        case ZPrime_CaT       => "z_G0304_CaT_G0309"

object gmosnorthfilter extends GmosNorthFilterSyntax

trait GmosNorthFpuSyntax:
  import lucuma.core.enums.GmosNorthFpu
  import lucuma.core.enums.GmosNorthFpu.*
  extension (self: GmosNorthFpu)
    def ocs2Tag: String =
      self match
        case Ifu2Slits     => "IFU_1"
        case IfuBlue       => "IFU_2"
        case IfuRed        => "IFU_3"
        case Ns0           => "NS_0"
        case Ns1           => "NS_1"
        case Ns2           => "NS_2"
        case Ns3           => "NS_3"
        case Ns4           => "NS_4"
        case Ns5           => "NS_5"
        case LongSlit_0_25 => "LONGSLIT_1"
        case LongSlit_0_50 => "LONGSLIT_2"
        case LongSlit_0_75 => "LONGSLIT_3"
        case LongSlit_1_00 => "LONGSLIT_4"
        case LongSlit_1_50 => "LONGSLIT_5"
        case LongSlit_2_00 => "LONGSLIT_6"
        case LongSlit_5_00 => "LONGSLIT_7"

object gmosnorthfpu extends GmosNorthFpuSyntax

trait GmosSouthFpuSyntax:
  import lucuma.core.enums.GmosSouthFpu
  import lucuma.core.enums.GmosSouthFpu.*
  extension (self: GmosSouthFpu)
    def ocs2Tag: String =
      self match {
        case Ifu2Slits           => "IFU_1"
        case IfuNS2Slits         => "IFU_N"
        case IfuBlue | IfuNSBlue => "IFU_2"
        case IfuRed | IfuNSRed   => "IFU_3"
        case Ns1                 => "NS_1"
        case Ns2                 => "NS_2"
        case Ns3                 => "NS_3"
        case Ns4                 => "NS_4"
        case Ns5                 => "NS_5"
        case LongSlit_0_25       => "LONGSLIT_1"
        case LongSlit_0_50       => "LONGSLIT_2"
        case LongSlit_0_75       => "LONGSLIT_3"
        case LongSlit_1_00       => "LONGSLIT_4"
        case LongSlit_1_50       => "LONGSLIT_5"
        case LongSlit_2_00       => "LONGSLIT_6"
        case LongSlit_5_00       => "LONGSLIT_7"
      }

object gmossouthfpu extends GmosSouthFpuSyntax

trait Flamingos2DisperserSyntax:
  import lucuma.core.enums.Flamingos2Disperser
  extension (self: Flamingos2Disperser) def ocs2Tag: String = self.shortName

object flamingos2disperser extends Flamingos2DisperserSyntax

trait Flamingos2FilterSyntax:
  import lucuma.core.enums.Flamingos2Filter
  extension (self: Flamingos2Filter)
    def ocs2Tag: String =
      self match
        case Flamingos2Filter.JLow   => "J_LOW"
        case Flamingos2Filter.KLong  => "K_LONG"
        case Flamingos2Filter.KShort => "K_SHORT"
        case Flamingos2Filter.KBlue  => "K_BLUE"
        case Flamingos2Filter.KRed   => "K_RED"
        case _                       => self.tag

object flamingos2filter extends Flamingos2FilterSyntax

trait Flamingos2FpuSyntax:
  import lucuma.core.enums.Flamingos2Fpu
  import lucuma.core.enums.Flamingos2Fpu.*
  extension (self: Flamingos2Fpu)
    def ocs2Tag: String =
      self match
        case Pinhole       => "PINHOLE"
        case SubPixPinhole => "SUBPIX_PINHOLE"
        case LongSlit1     => "LONGSLIT_1"
        case LongSlit2     => "LONGSLIT_2"
        case LongSlit3     => "LONGSLIT_3"
        case LongSlit4     => "LONGSLIT_4"
        case LongSlit6     => "LONGSLIT_6"
        case LongSlit8     => "LONGSLIT_8"

object flamingos2fpu extends Flamingos2FpuSyntax
