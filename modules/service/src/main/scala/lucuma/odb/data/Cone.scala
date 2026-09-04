// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.math.Angle
import lucuma.core.math.Coordinates

/** A cone on the sky: the positions within angular `distance` of `center`.
 *  Mirrors the `WhereCone` GraphQL input.
 *
 *  `distance` is at most 180°, enforced by construction: a separation cannot exceed
 *  that, and the SQL cone search misbehaves beyond it. `Angle` normalizes to
 *  [0°, 360°), so a "negative" angle arrives as its 360° complement and is likewise
 *  out of range.
 */
case class Cone private (center: Coordinates, distance: Angle)

object Cone:
  def from(center: Coordinates, distance: Angle): Option[Cone] =
    Option.when(distance.toMicroarcseconds <= Angle.Angle180.toMicroarcseconds)(new Cone(center, distance))
