// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import grackle.Result
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.User
import lucuma.odb.data.Cone
import lucuma.odb.util.Codecs.configuration_request_id
import lucuma.odb.util.Codecs.declination
import lucuma.odb.util.Codecs.right_ascension
import skunk.syntax.all.*

// Service-level tests for the SC-9240 cone search: they call `coneCandidates` directly, so
// only the SQL is under test.
//
// The GraphQL filter is covered by configurationRequests_targetCoordinates.
//
// Expected matches always come from lucuma-core's `angularDistance` (see ConeSearchFixture),
// never from hardcoded ids, making these differential tests.
class coneCandidates extends OdbSuite with ObservingModeSetupOperations with ConeSearchFixture:

  val admin = TestUsers.Standard.admin(1, 30)

  val piGeneral   = TestUsers.Standard.pi(2, 31)
  val piPole      = TestUsers.Standard.pi(3, 32)
  val piHalfWidth = TestUsers.Standard.pi(4, 33)
  val piOwner     = TestUsers.Standard.pi(5, 34)
  val piUnlinked  = TestUsers.Standard.pi(6, 35)
  val piBoundary  = TestUsers.Standard.pi(7, 36)

  val validUsers = List(admin, piGeneral, piPole, piHalfWidth, piOwner, piUnlinked, piBoundary)

  // 18' west of RA 0: even a small cone at the origin has to wrap to reach it.
  private val tightSeam: Coordinates =
    coords("23:58:48 +10:00:00")

  // Inside their cones but outside the small-angle `r / cos dec0` box
  private val halfWidthRegressions: List[Coordinates] = List(
    coords("15:42:20.52 +88:57:32.4"), // 0.9996° from (12h, +88.9°); ΔRA 55.6° > 52.1° box
    coords("02:55:59.88 +54:16:12"),   // 29.42° from (0h, +45°); ΔRA 44.0° > 42.4° box
  )

  // Each pole is one point on the sky, so a cone must match it from any RA.
  private val exactPoles: List[Coordinates] = List(
    coords("07:13:00 +90:00:00"),
    coords("19:47:00 -90:00:00"),
  )

  /** Seeds a program with one configuration request per position, paired with the position
   *  that decides whether a cone matches it. Reference coordinates come from the
   *  observation's asterism, so seeding a target is what fills the columns the SQL reads.
   */
  private def seed(as: User, positions: List[Coordinates]): IO[List[(ConfigurationRequest.Id, Coordinates)]] =
    for
      cfpid  <- createGeminiCallForProposalsAs(admin)
      pid    <- createProgramAs(as)
      _      <- addProposal(as, pid, Some(cfpid), None)
      seeded <- positions.traverse: c =>
                  for
                    tid <- createSiderealTargetAtAs(as, pid, c)
                    oid <- createGmosNorthLongSlitObservationAs(as, pid, List(tid))
                    cid <- createConfigurationRequestAs(as, oid)
                  yield (cid, c)
    yield seeded

  private def assertCone(seeded: List[(ConfigurationRequest.Id, Coordinates)], as: User)(cone: Cone): IO[Unit] =
    val expected = within(seeded)(cone.center, cone.distance)
    withServices(as): svc =>
      svc.configurationService.coneCandidates(cone)
        .flatMap(_.get.map(_.toSet))
        .map(assertEquals(_, expected, clue = cone))

  test("general geometry: box prefilter, exact trim, and the RA seam"):
    val cones: List[Cone] = List(
      cone(coords("00:00:00 +10:00:00"), 5.degrees),   // the in-box-outside-circle target
                                                       // leaks unless the trim runs
      cone(coords("00:00:00 +10:00:00"), 25.degrees),  // wide enough to wrap across RA 0
      cone(coords("00:00:00 +10:00:00"), 6.arcmin),    // the center target alone
      cone(coords("12:00:00 +89:00:00"), 5.degrees),
      cone(coords("06:00:00 +40:00:00"), 1.degrees),   // away from the origin cluster
      cone(coords("00:00:00 -10:00:00"), 30.degrees),  // southern hemisphere
    )
    for
      seeded <- seed(piGeneral, tightSeam :: basePositions)
      _      <- cones.traverse_(assertCone(seeded, piGeneral))
    yield ()

  test("pole cases: the dec box wraps and the RA box opens up"):
    val cones: List[Cone] = List(
      // Centered on a pole, where cos(dec0) is 0: the pole branch must short-circuit before
      // the asin half-width divides by it. The dec box also runs past +/-90 into the angle
      // encoding's wrap.
      cone(coords("00:00:00 +90:00:00"), 2.degrees),  // reaches (12h, +89°) too
      cone(coords("00:00:00 +90:00:00"), 30.arcmin),  // only the pole itself
      cone(coords("12:00:00 -90:00:00"), 2.degrees),  // same, south
      // 1° from the pole target but on the far side in RA, so an RA box that did not open up
      // near the pole would lose it.
      cone(coords("19:13:00 +89:00:00"), 2.degrees),
    )
    for
      seeded <- seed(piPole, exactPoles ++ basePositions)
      _      <- cones.traverse_(assertCone(seeded, piPole))
    yield ()

  test("RA prefilter half-width is the exact asin bound, not the small-angle one"):
    val cones: List[Cone] = List(
      cone(coords("12:00:00 +88:54:00"), 1.degrees),  // just inside the pole guard (88.9 + 1 < 90)
      cone(coords("00:00:00 +45:00:00"), 30.degrees), // mid dec, wide
    )
    for
      seeded <- seed(piHalfWidth, halfWidthRegressions ++ basePositions)
      _      <- cones.traverse_(assertCone(seeded, piHalfWidth))
    yield ()

  /** The reference coordinates actually stored for a request. The tracking pipeline that
   *  fills them (`CompositeTracking` via `Coordinates.centerOf`) round-trips through
   *  cartesian and truncates back to µas, so they can sit 1 µas off the seeded target --
   *  exact-match tests must aim at these, not at the seed.
   */
  private def storedCoordinates(cid: ConfigurationRequest.Id): IO[Coordinates] =
    withSession: s =>
      s.unique(
        sql"""select c_reference_ra, c_reference_dec from t_configuration_request
              where c_configuration_request_id = $configuration_request_id"""
          .query(right_ascension *: declination)
      )(cid).map(Coordinates.apply.tupled)

  // The regression the haversine trim fixes: the law-of-cosines form is flat near zero
  // separation, so a zero-radius cone could exclude its own center to float rounding.
  // Zero-radius cones aim at the *stored* coordinates (see `storedCoordinates`), and the
  // boundary cones are 1 µas either side of the measured separation, so every assertion
  // is deterministic at the data's own resolution.
  test("zero-radius and one-µas-boundary cones are deterministic"):
    val positions = List(
      coords("00:00:00 +10:00:00"),
      coords("00:00:00 +15:00:00"), // ~5° north of the first
    )
    for
      seeded <- seed(piBoundary, positions)
      stored <- seeded.traverse((cid, _) => storedCoordinates(cid).tupleLeft(cid))
      // A zero-radius cone on each stored position matches exactly that request.
      _      <- stored.traverse_((_, c) => assertCone(stored, piBoundary)(cone(c, Angle.Angle0)))
      // Straddle the measured separation by 1 µas: just-over includes the far request,
      // just-under excludes it.
      sep     = stored(0)._2.angularDistance(stored(1)._2)
      _      <- assertCone(stored, piBoundary)(cone(stored(0)._2, sep + Angle.fromMicroarcseconds(1)))
      _      <- assertCone(stored, piBoundary)(cone(stored(0)._2, sep - Angle.fromMicroarcseconds(1)))
    yield ()

  private val wide: Cone = cone(coords("00:00:00 +10:00:00"), 25.degrees)

  private val nearWide: List[Coordinates] =
    List(coords("00:00:00 +10:00:00"), coords("00:30:00 +14:00:00"))

  test("staff and above can reach into any program"):
    for
      seeded <- seed(piOwner, nearWide)
      ids    <- withServices(admin): svc =>
                  svc.configurationService.coneCandidates(wide).flatMap(_.get)
    yield
      // Being unscoped, admin also sees what the other tests seeded inside this cone.
      val ours = seeded.map(_._1).toSet
      assertEquals(ids.toSet.intersect(ours), within(seeded)(wide.center, wide.distance))

  test("a PI not linked to the program sees no candidates"):
    for
      _   <- seed(piOwner, nearWide)
      ids <- withServices(piUnlinked): svc =>
               svc.configurationService.coneCandidates(wide).flatMap(_.get)
    yield assertEquals(ids, Nil, clue = "unlinked PI sees no candidates")

  // Truncating would return wrong-but-plausible results, so the statement selects one row
  // over the cap and fails on it.
  test("more matches than `max` is a loud failure, not a truncation"):
    for
      _ <- seed(piOwner, nearWide)
      _ <- withServices(piOwner): svc =>
             svc.configurationService.coneCandidates(wide, max = 1).map:
               case Result.Failure(ps) => assert(ps.exists(_.message.contains("narrow the cone")), clue = ps)
               case other              => fail(s"expected a failure, got: $other")
    yield ()
