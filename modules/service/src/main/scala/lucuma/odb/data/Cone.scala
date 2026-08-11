// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import lucuma.core.math.Angle
import lucuma.core.math.Coordinates

/** A cone on the sky: the positions within angular `distance` of `center`.
 *  Mirrors the `WhereCone` GraphQL input.
 */
case class Cone(center: Coordinates, distance: Angle)
