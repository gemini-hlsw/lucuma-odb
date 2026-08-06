// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.all.*
import lucuma.core.math.Angle
import lucuma.core.math.Angle.toMicroarcseconds
import lucuma.core.math.Coordinates

// GraphQL-level test for the SC-9240 `targetCoordinates` cone WHERE filter,
// resolved out of band by the pre-compilation entry-rewrite. Expected matches
// are computed independently with lucuma-core's exact `angularDistance`.
class configurationRequests_targetCoordinates extends OdbSuite with ObservingModeSetupOperations {

  val pi    = TestUsers.Standard.pi(1, 30)
  val admin = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  private def coord(raHours: Double, decDegrees: Double): Coordinates =
    Coordinates.unsafeFromRadians(raHours * math.Pi / 12.0, decDegrees * math.Pi / 180.0)

  private val positions: List[(Double, Double)] = List(
    (0.0,  10.0),   // at the first cone's center
    (6.0,  40.0),   // far away
    (23.0, 10.0),   // across the RA 0/2π seam
    (0.5,  14.0),   // in-box but outside a small circle at the origin
    (12.0, 89.0),   // near the north pole
    (0.0, -10.0),   // south
  )

  test("targetCoordinates cone filter (normal, seam, exact-vs-box)"):
    for
      cfpid  <- createGeminiCallForProposalsAs(admin)
      pid    <- createProgramAs(pi)
      _      <- addProposal(pi, pid, Some(cfpid), None)
      seeded <- positions.traverse { case (rah, decd) =>
        for
          tid <- createSiderealTargetAtAs(pi, pid, rah.toString, decd.toString)
          oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
          cid <- createConfigurationRequestAs(pi, oid)
        yield (cid, coord(rah, decd))
      }

      // 5° cone at (0h, +10°): matches the center target; excludes the in-box-but-
      // outside-circle target, the seam target, and everything far.
      small   <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 18000 } }""")
      // 20° cone at (0h, +10°): now also reaches across the seam.
      seam    <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 75600 } }""")
      // small cone at the pole target.
      pole    <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "12.0" }, dec: { degrees: "89.0" } }, distance: { arcseconds: 7200 } }""")
    yield
      val center = coord(0.0, 10.0)
      def within(deg: Double): Set[?] =
        seeded.collect { case (cid, x) if center.angularDistance(x).toMicroarcseconds <= Angle.fromDoubleDegrees(deg).toMicroarcseconds => cid }.toSet

      assertEquals(small.toSet, within(5.0))
      assertEquals(seam.toSet,  within(21.0))
      val poleCenter = coord(12.0, 89.0)
      val poleExpected = seeded.collect { case (cid, x) if poleCenter.angularDistance(x).toMicroarcseconds <= Angle.fromDoubleDegrees(2.0).toMicroarcseconds => cid }.toSet
      assertEquals(pole.toSet, poleExpected)

}
