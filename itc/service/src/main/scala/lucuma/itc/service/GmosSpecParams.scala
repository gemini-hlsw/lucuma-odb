// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service

import io.circe.Encoder
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.math.Angle
import lucuma.itc.service.syntax.*

case class GmosNorthFpuParam(
  builtin: GmosNorthFpu
) derives Encoder.AsObject {
  def isIfu: Boolean            = builtin.isGNIfu
  def effectiveSlitWidth: Angle = builtin.effectiveSlitWidth
}

case class GmosSouthFpuParam(
  builtin: GmosSouthFpu
) derives Encoder.AsObject {
  def isIfu: Boolean            = builtin.isGSIfu
  def effectiveSlitWidth: Angle = builtin.effectiveSlitWidth
}
