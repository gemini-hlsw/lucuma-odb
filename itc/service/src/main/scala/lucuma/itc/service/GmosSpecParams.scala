// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import cats.Hash
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.math.Angle
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.itc.service.syntax.*

case class GmosNorthFpuParam(fpu: GmosFpuMask[GmosNorthFpu]):
  def isIfu: Boolean =
    fpu.builtinFpu.exists(_.isGNIfu)

  def effectiveSlitWidth: Angle =
    fpu.fold(b => b.value.effectiveSlitWidth, c => c.slitWidth.width)

object GmosNorthFpuParam:
  given Hash[GmosNorthFpuParam] = Hash.fromUniversalHashCode

case class GmosSouthFpuParam(fpu: GmosFpuMask[GmosSouthFpu]):
  def isIfu: Boolean =
    fpu.builtinFpu.exists(_.isGSIfu)

  def effectiveSlitWidth: Angle =
    fpu.fold(b => b.value.effectiveSlitWidth, c => c.slitWidth.width)

object GmosSouthFpuParam:
  given Hash[GmosSouthFpuParam] = Hash.fromUniversalHashCode
