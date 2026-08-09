// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.math.Angle
import lucuma.core.math.Angle.toMicroarcseconds
import lucuma.core.math.Coordinates
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program

// GraphQL-level tests for the SC-9240 `targetCoordinates` cone WHERE filter. Geometry is
// checked against lucuma-core's exact `angularDistance`; the rest cover how the cone
// survives compilation, since it is elaborated to a placeholder predicate and resolved
// afterwards (see ConeFilter).
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

  private def requestAt(pid: Program.Id, ra: String, dec: String): IO[ConfigurationRequest.Id] =
    for
      tid <- createSiderealTargetAtAs(pi, pid, ra, dec)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      cid <- createConfigurationRequestAs(pi, oid)
    yield cid

  test("targetCoordinates cone filter (normal, seam, exact-vs-box)"):
    for
      cfpid  <- createGeminiCallForProposalsAs(admin)
      pid    <- createProgramAs(pi)
      _      <- addProposal(pi, pid, Some(cfpid), None)
      seeded <- positions.traverse { case (rah, decd) =>
        requestAt(pid, rah.toString, decd.toString).map(cid => (cid, coord(rah, decd)))
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

  // --- how the cone survives compilation ---
  //
  // The cone is elaborated to a placeholder predicate and resolved afterwards, so grackle
  // substitutes variables and spreads fragments before it is ever parsed. These are the
  // shapes an earlier document-rewriting version got wrong: it looked for the literal
  // string `targetCoordinates` in the query text, so a cone arriving by variable was
  // silently ignored and unrelated variables were dropped on recompile.

  // One request inside the 5° cone at (0h, +10°) and one well outside it.
  private def coneSetup: IO[(Program.Id, ConfigurationRequest.Id, ConfigurationRequest.Id)] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None)
      near  <- requestAt(pid, "0.0", "10.0")
      far   <- requestAt(pid, "6.0", "40.0")
    yield (pid, near, far)

  private def ids(json: Json): IO[List[ConfigurationRequest.Id]] =
    json.hcursor
      .downFields("configurationRequests", "matches")
      .values.toList.flatten
      .traverse(_.hcursor.downField("id").as[ConfigurationRequest.Id])
      .leftMap(f => new RuntimeException(f.message))
      .liftTo[IO]

  private val ConeJson: Json =
    json"""{
      "center": { "ra": { "hours": "0.0" }, "dec": { "degrees": "10.0" } },
      "distance": { "arcseconds": 18000 }
    }"""

  private val ConeText: String =
    """targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 18000 } }"""

  test("cone supplied as the whole WHERE variable"):
    for
      (pid, near, _) <- coneSetup
      res <- query(
               pi,
               """query($where: WhereConfigurationRequest!) {
                    configurationRequests(WHERE: $where) { matches { id } }
                  }""",
               JsonObject(
                 "where" -> json"""{ "program": { "id": { "EQ": $pid } }, "targetCoordinates": $ConeJson }"""
               ).some
             )
      got <- ids(res)
    yield assertEquals(got, List(near))

  test("cone value itself supplied as a variable"):
    for
      (pid, near, _) <- coneSetup
      res <- query(
               pi,
               """query($cone: WhereCone!, $pid: ProgramId!) {
                    configurationRequests(
                      WHERE: { program: { id: { EQ: $pid } }, targetCoordinates: $cone }
                    ) { matches { id } }
                  }""",
               JsonObject("pid" -> pid.asJson, "cone" -> ConeJson).some
             )
      got <- ids(res)
    yield assertEquals(got, List(near))

  test("literal cone alongside an unrelated variable"):
    for
      (pid, near, _) <- coneSetup
      res <- query(
               pi,
               s"""query($$lim: NonNegInt!) {
                     configurationRequests(
                       WHERE: { program: { id: { EQ: "$pid" } }, $ConeText },
                       LIMIT: $$lim
                     ) { matches { id } }
                   }""",
               JsonObject("lim" -> json"""1000""").some
             )
      got <- ids(res)
    yield assertEquals(got, List(near))

  test("cone inside a fragment"):
    for
      (pid, near, _) <- coneSetup
      res <- query(
               pi,
               s"""query {
                     configurationRequests(WHERE: { program: { id: { EQ: "$pid" } }, $ConeText }) {
                       matches { ...cid }
                     }
                   }
                   fragment cid on ConfigurationRequest { id }"""
             )
      got <- ids(res)
    yield assertEquals(got, List(near))

  test("cone under OR keeps its position"):
    for
      (pid, near, far) <- coneSetup
      // The other arm selects the request *outside* the cone, so hoisting the cone up to
      // a top-level conjunct would wrongly drop it. Both arms must survive.
      res <- query(
               pi,
               s"""query {
                     configurationRequests(WHERE: {
                       program: { id: { EQ: "$pid" } },
                       OR: [ { $ConeText }, { id: { EQ: "$far" } } ]
                     }) { matches { id } }
                   }"""
             )
      got <- ids(res)
    yield assertEquals(got.toSet, Set(near, far))

}
