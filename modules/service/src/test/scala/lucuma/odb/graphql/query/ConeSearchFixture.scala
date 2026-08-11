// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import lucuma.core.math.Angle
import lucuma.core.math.Angle.toMicroarcseconds
import lucuma.core.math.Coordinates

// Shared geometry for the SC-9240 cone-search tests: position fixtures and the
// expected-set reference, computed with lucuma-core's exact `angularDistance`.
trait ConeSearchFixture:

  def coord(raHours: Double, decDegrees: Double): Coordinates =
    Coordinates.unsafeFromRadians(raHours * math.Pi / 12.0, decDegrees * math.Pi / 180.0)

  def deg(d: Double): Angle =
    Angle.fromDoubleDegrees(d)

  // (RA hours, Dec degrees) — chosen to exercise normal, seam, pole, and
  // box-corner (in-box but out-of-circle) cases.
  val basePositions: List[(Double, Double)] = List(
    (0.0,   10.0),   // near origin
    (6.0,   40.0),   // far away
    (23.0,  10.0),   // RA 345°, across the 0/2π seam from the origin
    (0.5,   14.0),   // offset; lands in-box but outside a small circle at the origin
    (12.0,  89.0),   // near the north pole
    (0.0,  -10.0),   // south of origin
  )

  def within[A](seeded: List[(A, Coordinates)])(center: Coordinates, distance: Angle): Set[A] =
    seeded.collect {
      case (a, c) if center.angularDistance(c).toMicroarcseconds <= distance.toMicroarcseconds => a
    }.toSet
