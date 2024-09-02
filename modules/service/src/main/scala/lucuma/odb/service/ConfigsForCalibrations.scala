// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.derived.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.Wavelength
import lucuma.odb.service.Services.Syntax.*

sealed trait ConfigForCalibrations derives Eq

object ConfigForCalibrations {
  case class GmosNConfigs(g: GmosNorthGrating, f: Option[GmosNorthFilter], u: GmosNorthFpu, w: Wavelength, x: GmosXBinning, y: GmosYBinning) extends ConfigForCalibrations derives Eq

  case class GmosSConfigs(g: GmosSouthGrating, f: Option[GmosSouthFilter], u: GmosSouthFpu, w: Wavelength, x: GmosXBinning, y: GmosYBinning) extends ConfigForCalibrations derives Eq
}
