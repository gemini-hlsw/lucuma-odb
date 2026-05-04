// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.visitor

import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.odb.sequence.util.HashBytes

final case class Config(
  mode: VisitorObservingModeType,
  centralWavelength: Wavelength,
  guideStarMinSep: Angle 
)

object Config:

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      Array.concat(
        HashBytes[VisitorObservingModeType].hashBytes(c.mode),
        HashBytes[Wavelength].hashBytes(c.centralWavelength),
        HashBytes[Angle].hashBytes(c.guideStarMinSep),
      )


