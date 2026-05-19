// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.ghost.ifu

import lucuma.core.enums.GhostResolutionMode
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.PosAngleConstraint
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

case class StaticContext(
  resolutionMode:     GhostResolutionMode,
  sky:                Option[Coordinates],
  posAngleConstraint: PosAngleConstraint,
  explicitBase:       Option[Coordinates],
  when:               Timestamp,
  svcExposureTime:    Option[TimeSpan]
):
  // Maybe we should limit GHOST to Fixed?
  val angle: Option[Angle] =
    PosAngleConstraint.angle.getOption(posAngleConstraint)


object StaticContext:

  def apply(
    resolutionMode:  GhostResolutionMode,
    sky:             Option[Coordinates],
    now:             Timestamp,
    obsTime:         Option[Timestamp],
    pac:             PosAngleConstraint,
    expBase:         Option[Coordinates],
    svcExposureTime: Option[TimeSpan]
  ): StaticContext =
    StaticContext(
      resolutionMode,
      sky,
      pac,
      expBase,
      obsTime.getOrElse(now),
      svcExposureTime
    )