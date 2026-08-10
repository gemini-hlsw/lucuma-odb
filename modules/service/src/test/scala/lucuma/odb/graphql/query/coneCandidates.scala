// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.Angle.toMicroarcseconds
import lucuma.core.math.Coordinates
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User

// Direct service-level test for the SC-9240 cone (angular-distance) search.
// Expected matches are computed independently with lucuma-core's exact
// `angularDistance`, so this validates the pure-SQL box-prefilter + exact trim
// (including the RA 0/2π seam and the pole case) against a reference.
class coneCandidates extends OdbSuite with ObservingModeSetupOperations {

  val pi    = TestUsers.Standard.pi(1, 30)
  val admin = TestUsers.Standard.admin(2, 31)
  val pi2   = TestUsers.Standard.pi(3, 32)
  val validUsers = List(pi, admin, pi2)

  private def coord(raHours: Double, decDegrees: Double): Coordinates =
    Coordinates.unsafeFromRadians(raHours * math.Pi / 12.0, decDegrees * math.Pi / 180.0)

  private def deg(d: Double): Angle =
    Angle.fromDoubleDegrees(d)

  // (RA hours, Dec degrees) — chosen to exercise normal, seam, pole, and
  // box-corner (in-box but out-of-circle) cases.
  private val positions: List[(Double, Double)] = List(
    (0.0,   10.0),   // near origin
    (6.0,   40.0),   // far away
    (23.0,  10.0),   // RA 345°, across the 0/2π seam from the origin
    (0.5,   14.0),   // offset; lands in-box but outside a small circle at the origin
    (12.0,  89.0),   // near the north pole
    (0.0,  -10.0),   // south of origin
    (23.98, 10.0),   // just west of RA 0 (tight seam)
    // Regressions for the RA prefilter half-width, asin(sin r / cos dec0): each is
    // inside its cone but outside the small-angle r / cos dec0 box, so an
    // undershooting prefilter loses it. Neither cone takes the pole shortcut.
    (15.7057, 88.959), // 0.9996° from (12h, +88.9°); ΔRA 55.6° > 52.1° small-angle box
    (2.9333,  54.27),  // 29.42° from (0h, +45°); ΔRA 44.0° > 42.4° small-angle box
  )

  private def assertCone(seeded: List[(ConfigurationRequest.Id, Coordinates)], as: User = pi)(center: Coordinates, distance: Angle): IO[Unit] =
    val expected = seeded.collect {
      case (cid, c) if center.angularDistance(c).toMicroarcseconds <= distance.toMicroarcseconds => cid
    }.toSet
    withServices(as): svc =>
      svc.configurationService.coneCandidates(center, distance)
        .flatMap(_.get.map(_.toSet))
        .map(actual => assertEquals(actual, expected))

  test("coneCandidates matches lucuma-core angularDistance across cones"):
    val cones: List[(Coordinates, Angle)] = List(
      (coord(0.0, 10.0),  deg(5.0)),
      (coord(0.0, 10.0),  deg(25.0)),
      (coord(12.0, 89.0), deg(5.0)),
      (coord(6.0, 40.0),  deg(1.0)),
      (coord(0.0, -10.0), deg(30.0)),
      (coord(0.0, 10.0),  deg(0.1)),
      (coord(12.0, 88.9), deg(1.0)),  // high-dec, just inside the pole guard (88.9 + 1 < 90)
      (coord(0.0, 45.0),  deg(30.0)), // mid-dec wide cone
    )
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
      _      <- cones.traverse_ { case (c, d) => assertCone(seeded)(c, d) }

      // Candidates are scoped by program visibility: staff-and-above are unscoped,
      // while a PI with no link to the program gets nothing.
      wide    = (coord(0.0, 10.0), deg(25.0))
      _      <- assertCone(seeded, as = admin)(wide._1, wide._2)
      _      <- withServices(pi2): svc =>
                  svc.configurationService.coneCandidates(wide._1, wide._2)
                    .flatMap(_.get)
                    .map(ids => assertEquals(ids, Nil, clue = "unlinked PI sees no candidates"))

      // More matches than `max` is a loud failure, not a truncation.
      _      <- withServices(pi): svc =>
                  svc.configurationService.coneCandidates(wide._1, wide._2, max = 1).map:
                    case Result.Failure(ps) => assert(ps.exists(_.message.contains("narrow the cone")), clue = ps)
                    case other              => fail(s"expected a failure, got: $other")
    yield ()

}
