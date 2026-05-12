// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.visitor

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.util.HashBytes

final case class Config(
  mode: VisitorObservingModeType,
  centralWavelength: Wavelength,
  // Science field of view, understood as the diameter of a circular area.
  scienceFov: Angle,
  name: Option[NonEmptyString],
  totalRequestTime: Option[TimeSpan]
)

object Config:

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      Array.concat(
        HashBytes[VisitorObservingModeType].hashBytes(c.mode),
        HashBytes[Wavelength].hashBytes(c.centralWavelength),
        HashBytes[Angle].hashBytes(c.scienceFov),
        c.name.fold(Array.emptyByteArray)(n => HashBytes[String].hashBytes(n.value)),
        c.totalRequestTime.fold(Array.emptyByteArray)(HashBytes[TimeSpan].hashBytes)
      )
