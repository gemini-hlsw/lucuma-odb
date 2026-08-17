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
import lucuma.core.math.Coordinates
import lucuma.core.math.syntax.int.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.odb.data.OdbError

// GraphQL-level tests for the `targetCoordinates` cone WHERE filter
//
// Care is taken to consider several scenarios on how the targetCoordinates filters could be
// passed along, inline, in variables,etc
//
// The SQL itself is covered at the service level by coneCandidates.
class configurationRequests_targetCoordinates extends OdbSuite with ObservingModeSetupOperations with ConeSearchFixture {

  val pi    = TestUsers.Standard.pi(1, 30)
  val admin = TestUsers.Standard.admin(2, 31)
  val validUsers = List(pi, admin)

  private def requestAt(pid: Program.Id, coords: Coordinates): IO[ConfigurationRequest.Id] =
    for
      tid <- createSiderealTargetAtAs(pi, pid, coords)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      cid <- createConfigurationRequestAs(pi, oid)
    yield cid

  test("targetCoordinates cone filter (normal, seam, exact-vs-box)"):
    for
      cfpid  <- createGeminiCallForProposalsAs(admin)
      pid    <- createProgramAs(pi)
      _      <- addProposal(pi, pid, Some(cfpid), None)
      seeded <- basePositions.traverse { c =>
        requestAt(pid, c).map(cid => (cid, c))
      }

      // 5° at (0h, +10°): the center target only. The offset target sits in the bounding box
      // but outside the circle, so it leaks unless the exact trim runs.
      small   <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 18000 } }""")
      // 21° at the same center: wide enough to wrap across RA 0 and pick up the seam target.
      seam    <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 75600 } }""")
      // 2° at the near-pole target, where the RA box has to open up.
      pole    <- configurationRequestsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "12.0" }, dec: { degrees: "89.0" } }, distance: { arcseconds: 7200 } }""")
    yield
      val center = coords("00:00:00 +10:00:00")
      assertEquals(small.toSet, within(seeded)(center, 5.degrees))
      assertEquals(seam.toSet,  within(seeded)(center, 21.degrees))
      assertEquals(pole.toSet,  within(seeded)(coords("12:00:00 +89:00:00"), 2.degrees))

  // One request inside the 5° cone at (0h, +10°) and one well outside it.
  private def coneSetup: IO[(Program.Id, ConfigurationRequest.Id, ConfigurationRequest.Id)] =
    for
      cfpid <- createGeminiCallForProposalsAs(admin)
      pid   <- createProgramAs(pi)
      _     <- addProposal(pi, pid, Some(cfpid), None)
      near  <- requestAt(pid, coords("00:00:00 +10:00:00"))
      far   <- requestAt(pid, coords("06:00:00 +40:00:00"))
    yield (pid, near, far)

  private def ids(json: Json): IO[List[ConfigurationRequest.Id]] =
    json.hcursor
      .downFields("configurationRequests", "matches")
      .values.toList.flatten
      .traverse(_.hcursor.downField("id").as[ConfigurationRequest.Id])
      .leftMap(f => new RuntimeException(f.message))
      .liftTo[IO]

  // A 5° cone at (0h, +10°) as a JSON variable value
  private val ConeJson: Json =
    json"""{
      "center": { "ra": { "hours": "0.0" }, "dec": { "degrees": "10.0" } },
      "distance": { "arcseconds": 18000 }
    }"""

  // A 5° cone at (0h, +10°) as inline GraphQL.
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

  // The mutation shares this WHERE input, but rejects taking a cone filter.
  test("cone is rejected by the update mutation"):
    for
      (pid, _, _) <- coneSetup
      err <- expectOdbError(
               user = pi,
               query = s"""
                 mutation {
                   updateConfigurationRequests(input: {
                     SET: { status: APPROVED },
                     WHERE: { program: { id: { EQ: "$pid" } }, $ConeText }
                   }) { requests { id } }
                 }
               """,
               expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("targetCoordinates") => () }
             )
    yield err

  // A separation cannot exceed 180°.
  test("cone distance outside [0°, 180°] is rejected"):
    def reject(distance: String): IO[Unit] =
      expectOdbError(
        user = pi,
        query = s"""query {
            configurationRequests(WHERE: { targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { degrees: "$distance" } } }) {
              matches { id }
            }
          }""",
        expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("distance") => () }
      )
    reject("200.0") *> reject("-10.0")

  // Each distinct cone costs a candidate scan and can inject thousands of ids, so the
  // count per operation is capped (ConeFilter.MaxConesPerOperation).
  test("more than 5 distinct cones in one query is rejected"):
    def coneOfRadius(degrees: Int): String =
      s"""{ targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { degrees: "$degrees" } } }"""
    def whereOr(n: Int): String =
      s"""WHERE: { OR: [ ${(1 to n).map(coneOfRadius).mkString(", ")} ] }"""
    for
      _ <- expectOdbError(
             user = pi,
             query = s"query { configurationRequests(${whereOr(6)}) { matches { id } } }",
             expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("targetCoordinates") => () }
           )
      _ <- query(pi, s"query { configurationRequests(${whereOr(5)}) { matches { id } } }")
    yield ()

  // An empty candidate set becomes `In(idPath, Nil)`, which grackle compiles to false;
  // the main query must still run as a single statement and return no matches.
  test("cone matching nothing"):
    for
      (pid, _, _)   <- coneSetup
      (resp, stats) <- queryWithSqlStats(
                         pi,
                         s"""query {
                               configurationRequests(WHERE: { program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "12.0" }, dec: { degrees: "-45.0" } }, distance: { arcseconds: 10 } } }) {
                                 matches { id }
                               }
                             }"""
                       )
      got <- ids(resp.hcursor.downField("data").focus.getOrElse(Json.Null))
    yield
      assertEquals(got, Nil)
      assertEquals(stats.length, 1, clue = stats.map(_.normalize.sql))

  test("cone under OR keeps its position"):
    for
      (pid, near, far) <- coneSetup
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
