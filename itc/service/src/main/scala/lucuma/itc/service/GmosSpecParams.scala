// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.math.Angle
import lucuma.core.model.sequence.gmos.GmosFpuMask

case class GmosNorthFpuParam(fpu: GmosFpuMask[GmosNorthFpu]):
  /** How this focal plane unit gets its sky, when it is an IFU. */
  def ifuSky: Option[GmosIfuSky] =
    fpu.builtinFpu.collect:
      case GmosNorthFpu.Ifu2Slits                     => GmosIfuSky.TwoSlit
      case GmosNorthFpu.IfuBlue | GmosNorthFpu.IfuRed => GmosIfuSky.OneSlit

  def isIfu: Boolean =
    ifuSky.isDefined

  /**
   * Whether the legacy recipe traces two slits for this focal plane unit. Mirrors
   * `GmosNorth.isIfu2()`, which tests for IFU_1 alone.
   */
  def isTwoSlitIfu: Boolean =
    fpu.builtinFpu.contains(GmosNorthFpu.Ifu2Slits)

  def effectiveSlitWidth: Angle =
    fpu.fold(b => b.value.effectiveSlitWidth, c => c.slitWidth.width)

object GmosNorthFpuParam:
  given Hash[GmosNorthFpuParam] = Hash.fromUniversalHashCode

case class GmosSouthFpuParam(fpu: GmosFpuMask[GmosSouthFpu]):
  /**
   * See [[GmosNorthFpuParam.ifuSky]]. Only the South pairs nod & shuffle with the IFU: the North
   * has N&S slits (`Ns0`-`Ns5`) but no IFU_N focal plane units at all, which is also where
   * `FPUnitNorth.isNS` and `FPUnitSouth.isNS` diverge.
   */
  def ifuSky: Option[GmosIfuSky] =
    fpu.builtinFpu.collect:
      case GmosSouthFpu.Ifu2Slits                                                    => GmosIfuSky.TwoSlit
      case GmosSouthFpu.IfuBlue | GmosSouthFpu.IfuRed                                => GmosIfuSky.OneSlit
      case GmosSouthFpu.IfuNS2Slits | GmosSouthFpu.IfuNSBlue | GmosSouthFpu.IfuNSRed =>
        GmosIfuSky.NodAndShuffle

  def isIfu: Boolean =
    ifuSky.isDefined

  /**
   * See [[GmosNorthFpuParam.isTwoSlitIfu]]. `GmosSouth.isIfu2()` likewise tests for IFU_1 alone, so
   * the nod & shuffle two-slit unit is excluded even though it also has two slits.
   */
  def isTwoSlitIfu: Boolean =
    fpu.builtinFpu.contains(GmosSouthFpu.Ifu2Slits)

  def effectiveSlitWidth: Angle =
    fpu.fold(b => b.value.effectiveSlitWidth, c => c.slitWidth.width)

object GmosSouthFpuParam:
  given Hash[GmosSouthFpuParam] = Hash.fromUniversalHashCode
