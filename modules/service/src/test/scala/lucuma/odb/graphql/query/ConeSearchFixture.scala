// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import lucuma.core.math.Angle
import lucuma.core.math.Angle.toMicroarcseconds
import lucuma.core.math.Coordinates
import lucuma.odb.TestCoordinates

// Shared geometry utilities for cone-search tests
trait ConeSearchFixture:

  export TestCoordinates.coords

  val basePositions: List[Coordinates] = List(
    coords("00:00:00 +10:00:00"), // near origin
    coords("06:00:00 +40:00:00"), // far away
    coords("23:00:00 +10:00:00"), // RA 345°, across the 0/2π seam from the origin
    coords("00:30:00 +14:00:00"), // offset; lands in-box but outside a small circle at the origin
    coords("12:00:00 +89:00:00"), // near the north pole
    coords("00:00:00 -10:00:00"), // south of origin
  )

  def within[A](seeded: List[(A, Coordinates)])(center: Coordinates, distance: Angle): Set[A] =
    seeded.collect {
      case (a, c) if center.angularDistance(c).toMicroarcseconds <= distance.toMicroarcseconds => a
    }.toSet
