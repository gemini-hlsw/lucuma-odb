// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Coordinates
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User
import lucuma.odb.data.Cone

// Direct service-level test for the SC-9240 cone (angular-distance) search.
// Expected matches are computed independently with lucuma-core's exact
// `angularDistance` (see ConeSearchFixture), so this validates the pure-SQL
// box-prefilter + exact trim (including the RA 0/2π seam and the pole case)
// against a reference.
class coneCandidates extends OdbSuite with ObservingModeSetupOperations with ConeSearchFixture {

  val pi    = TestUsers.Standard.pi(1, 30)
  val admin = TestUsers.Standard.admin(2, 31)
  val pi2   = TestUsers.Standard.pi(3, 32)
  val validUsers = List(pi, admin, pi2)

  private val positions: List[Coordinates] = basePositions ++ List(
    coords("23:58:48 +10:00:00"),   // just west of RA 0 (tight seam)
    // Regressions for the RA prefilter half-width, asin(sin r / cos dec0): each is
    // inside its cone but outside the small-angle r / cos dec0 box, so an
    // undershooting prefilter loses it. Neither cone takes the pole shortcut.
    coords("15:42:20.52 +88:57:32.4"), // 0.9996° from (12h, +88.9°); ΔRA 55.6° > 52.1° small-angle box
    coords("02:55:59.88 +54:16:12"),   // 29.42° from (0h, +45°); ΔRA 44.0° > 42.4° small-angle box
    // Exactly on the poles, where cos(dec0) is 0 and RA is meaningless. The RA below is
    // arbitrary: both are one point on the sky, and a cone must match them from any RA.
    coords("07:13:00 +90:00:00"),
    coords("19:47:00 -90:00:00"),
  )

  private def assertCone(seeded: List[(ConfigurationRequest.Id, Coordinates)], as: User = pi)(cone: Cone): IO[Unit] =
    val expected = within(seeded)(cone.center, cone.distance)
    withServices(as): svc =>
      svc.configurationService.coneCandidates(cone)
        .flatMap(_.get.map(_.toSet))
        .map(actual => assertEquals(actual, expected))

  test("coneCandidates matches lucuma-core angularDistance across cones"):
    val cones: List[Cone] = List(
      Cone(coords("00:00:00 +10:00:00"), 5.degrees),
      Cone(coords("00:00:00 +10:00:00"), 25.degrees),
      Cone(coords("12:00:00 +89:00:00"), 5.degrees),
      Cone(coords("06:00:00 +40:00:00"), 1.degrees),
      Cone(coords("00:00:00 -10:00:00"), 30.degrees),
      Cone(coords("00:00:00 +10:00:00"), 6.arcmin),
      Cone(coords("12:00:00 +88:54:00"), 1.degrees),  // high-dec, just inside the pole guard (88.9 + 1 < 90)
      Cone(coords("00:00:00 +45:00:00"), 30.degrees), // mid-dec wide cone
      // Centered exactly on a pole: cos(dec0) is 0, so the asin half-width would divide by
      // zero -- the pole branch must short-circuit first. The dec box also runs past +/-90
      // into the angle encoding's wrap.
      Cone(coords("00:00:00 +90:00:00"), 2.degrees),  // reaches (12h, +89°) too
      Cone(coords("00:00:00 +90:00:00"), 30.arcmin),  // only the pole itself
      Cone(coords("12:00:00 -90:00:00"), 2.degrees),  // same, south
      // Just short of the pole, from the far side in RA: the pole target is 1° away
      // whatever its RA, so an RA-box that did not open up would lose it.
      Cone(coords("19:13:00 +89:00:00"), 2.degrees),
    )
    for
      cfpid  <- createGeminiCallForProposalsAs(admin)
      pid    <- createProgramAs(pi)
      _      <- addProposal(pi, pid, Some(cfpid), None)
      seeded <- positions.traverse { c =>
        for
          tid <- createSiderealTargetAtAs(pi, pid, c)
          oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
          cid <- createConfigurationRequestAs(pi, oid)
        yield (cid, c)
      }
      _      <- cones.traverse_(assertCone(seeded))

      // Candidates are scoped by program visibility: staff-and-above are unscoped,
      // while a PI with no link to the program gets nothing.
      wide    = Cone(coords("00:00:00 +10:00:00"), 25.degrees)
      _      <- assertCone(seeded, as = admin)(wide)
      _      <- withServices(pi2): svc =>
                  svc.configurationService.coneCandidates(wide)
                    .flatMap(_.get)
                    .map(ids => assertEquals(ids, Nil, clue = "unlinked PI sees no candidates"))

      // More matches than `max` is a loud failure, not a truncation.
      _      <- withServices(pi): svc =>
                  svc.configurationService.coneCandidates(wide, max = 1).map:
                    case Result.Failure(ps) => assert(ps.exists(_.message.contains("narrow the cone")), clue = ps)
                    case other              => fail(s"expected a failure, got: $other")
    yield ()

}
