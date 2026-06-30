// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.visitor

import cats.Eq
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.util.TimeSpan
import lucuma.odb.sequence.util.HashBytes

final case class Config(
  mode: VisitorObservingModeType,
  centralWavelength: Wavelength,
  // AGS field of view, understood as the diameter of a circular area.
  agsDiameter: Angle,
  name: Option[NonEmptyString],
  totalRequestTime: Option[TimeSpan]
):
  def validate: Either[String, Unit] =
    mode match
      case VisitorObservingModeType.VisitorNorth | VisitorObservingModeType.VisitorSouth
        if name.isEmpty || totalRequestTime.isEmpty =>
        Left(s"Visitor mode $mode requires both `name` and `totalRequestTime` to be provided.")
      case _ => Right(())

object Config:

  given Eq[Config] =
    Eq.by(a => (a.mode, a.centralWavelength, a.name, a.totalRequestTime))

  given HashBytes[Config] with
    def hashBytes(c: Config): Array[Byte] =
      Array.concat(
        HashBytes[VisitorObservingModeType].hashBytes(c.mode),
        HashBytes[Wavelength].hashBytes(c.centralWavelength),
        HashBytes[Angle].hashBytes(c.agsDiameter),
        c.name.fold(Array.emptyByteArray)(n => HashBytes[String].hashBytes(n.value)),
        c.totalRequestTime.fold(Array.emptyByteArray)(HashBytes[TimeSpan].hashBytes)
      )
