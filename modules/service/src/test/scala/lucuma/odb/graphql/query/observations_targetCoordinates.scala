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
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.ProperMotion
import lucuma.core.math.RightAscension
import lucuma.core.math.syntax.int.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.odb.data.OdbError

// GraphQL-level tests for the `targetCoordinates` cone WHERE filter on observations.
//
// The filter matches against the stored J2000 base position, computed by obscalc:
// the explicit base if set, otherwise the asterism composite PM-corrected to J2000.0.
// Observations whose asterism contains a non-sidereal or opportunity target have no
// position and are invisible to the filter (and so always match under NOT).
//
// The SQL geometry is shared with configuration requests (ConeSearch) and covered at
// the service level by coneCandidates; the binding/resolution plumbing is covered by
// configurationRequests_targetCoordinates. This suite covers what is specific to
// observations: the stored position's semantics and freshness, and the reachability
// of the filter through the WHEREs that embed WhereObservation.
class observations_targetCoordinates extends OdbSuite with ObservingModeSetupOperations with ConeSearchFixture {

  val pi      = TestUsers.Standard.pi(1, 30)
  val pi2     = TestUsers.Standard.pi(2, 32)
  val admin   = TestUsers.Standard.admin(3, 31)
  val service = TestUsers.service(4)
  val validUsers = List(pi, pi2, admin, service)

  private def raInput(c: Coordinates): String  = s"""ra: { hms: "${RightAscension.fromStringHMS.reverseGet(c.ra)}" }"""
  private def decInput(c: Coordinates): String = s"""dec: { dms: "${Declination.fromStringSignedDMS.reverseGet(c.dec)}" }"""

  private def coneText(center: Coordinates, arcseconds: Long): String =
    s"""targetCoordinates: { center: { ${raInput(center)}, ${decInput(center)} }, distance: { arcseconds: $arcseconds } }"""

  private def observationAt(pid: Program.Id, c: Coordinates): IO[Observation.Id] =
    for
      tid <- createSiderealTargetAtAs(pi, pid, c)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdateAs(service, pid, oid)
    yield oid

  private def ids(json: Json): IO[List[Observation.Id]] =
    json.hcursor
      .downFields("observations", "matches")
      .values.toList.flatten
      .traverse(_.hcursor.downField("id").as[Observation.Id])
      .leftMap(f => new RuntimeException(f.message))
      .liftTo[IO]

  test("targetCoordinates cone filter (normal, seam, exact-vs-box)"):
    for
      pid    <- createProgramAs(pi)
      seeded <- basePositions.traverse(c => observationAt(pid, c).map(oid => (oid, c)))
      // 5° at (0h, +10°): the center target only. The offset target sits in the bounding box
      // but outside the circle, so it leaks unless the exact trim runs.
      small  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 18000 } }""")
      // 21° at the same center: wide enough to wrap across RA 0 and pick up the seam target.
      seam   <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 75600 } }""")
      // 2° at the near-pole target, where the RA box has to open up.
      pole   <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, targetCoordinates: { center: { ra: { hours: "12.0" }, dec: { degrees: "89.0" } }, distance: { arcseconds: 7200 } }""")
    yield
      val center = coords("00:00:00 +10:00:00")
      assertEquals(small.toSet, within(seeded)(center, 5.degrees))
      assertEquals(seam.toSet,  within(seeded)(center, 21.degrees))
      assertEquals(pole.toSet,  within(seeded)(coords("12:00:00 +89:00:00"), 2.degrees))

  private val Near = coords("00:00:00 +10:00:00")
  private val Far  = coords("06:00:00 +40:00:00")

  // A 5° cone at (0h, +10°): matches Near, not Far.
  private val NearCone: String = coneText(Near, 18000L)

  // One observation inside the 5° cone at (0h, +10°) and one well outside it.
  private def coneSetup: IO[(Program.Id, Observation.Id, Observation.Id)] =
    for
      pid  <- createProgramAs(pi)
      near <- observationAt(pid, Near)
      far  <- observationAt(pid, Far)
    yield (pid, near, far)

  test("cone supplied as the whole WHERE variable"):
    for
      (pid, near, _) <- coneSetup
      res <- query(
               pi,
               """query($where: WhereObservation!) {
                    observations(WHERE: $where) { matches { id } }
                  }""",
               JsonObject(
                 "where" -> json"""{
                   "program": { "id": { "EQ": $pid } },
                   "targetCoordinates": {
                     "center": { "ra": { "hours": "0.0" }, "dec": { "degrees": "10.0" } },
                     "distance": { "arcseconds": 18000 }
                   }
                 }"""
               ).some
             )
      got <- ids(res)
    yield assertEquals(got, List(near))

  test("cone under OR keeps its position"):
    for
      (pid, near, far) <- coneSetup
      got <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, OR: [ { $NearCone }, { id: { EQ: "$far" } } ]""")
    yield assertEquals(got.toSet, Set(near, far))

  // An empty candidate set becomes `In(idPath, Nil)`, which grackle compiles to false;
  // the main query must still run as a single statement and return no matches.
  test("cone matching nothing"):
    for
      (pid, _, _)   <- coneSetup
      (resp, stats) <- queryWithSqlStats(
                         pi,
                         s"""query {
                               observations(WHERE: { program: { id: { EQ: "$pid" } }, ${coneText(coords("12:00:00 -45:00:00"), 10L)} }) {
                                 matches { id }
                               }
                             }"""
                       )
      got <- ids(resp.hcursor.downField("data").focus.getOrElse(Json.Null))
    yield
      assertEquals(got, Nil)
      assertEquals(stats.length, 1, clue = stats.map(_.normalize.sql))

  // The two cone entities dispatch to different candidate lookups; one operation
  // may use both.
  test("observation and configuration-request cones in one operation"):
    for
      (pid, near, _) <- coneSetup
      res <- query(
               pi,
               s"""query {
                     observations(WHERE: { program: { id: { EQ: "$pid" } }, $NearCone }) { matches { id } }
                     configurationRequests(WHERE: { program: { id: { EQ: "$pid" } }, $NearCone }) { matches { id } }
                   }"""
             )
      got <- ids(res)
    yield
      assertEquals(got, List(near))
      assertEquals(res.hcursor.downFields("configurationRequests", "matches").values.toList.flatten, Nil)

  // An observation with an opportunity target has no stored position: it never
  // matches a cone, and consequently always matches the cone's negation.
  test("opportunity-target observation: invisible to the cone, matches under NOT"):
    for
      (pid, near, far) <- coneSetup
      tid  <- createOpportunityTargetAs(pi, pid)
      opp  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdateAs(service, pid, opp)
      pos  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, $NearCone""")
      neg  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, NOT: { $NearCone }""")
    yield
      assertEquals(pos, List(near))
      assertEquals(neg.toSet, Set(far, opp))

  // A mixed asterism (sidereal + opportunity) gets no stored position either: a
  // center computed from the sidereal subset would misrepresent the pointing.
  test("mixed sidereal + opportunity asterism has no position"):
    for
      pid  <- createProgramAs(pi)
      sid  <- createSiderealTargetAtAs(pi, pid, Near)
      opp  <- createOpportunityTargetAs(pi, pid)
      oid  <- createGmosNorthLongSlitObservationAs(pi, pid, List(sid, opp))
      _    <- runObscalcUpdateAs(service, pid, oid)
      got  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, $NearCone""")
    yield assertEquals(got, Nil)

  private def setExplicitBase(oid: Observation.Id, c: Coordinates): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateObservations(input: {
              SET: { targetEnvironment: { explicitBase: { ${raInput(c)}, ${decInput(c)} } } },
              WHERE: { id: { EQ: ${oid.asJson} } }
            }) { observations { id } }
          }"""
    ).void

  test("explicit base overrides the asterism"):
    for
      pid <- createProgramAs(pi)
      tid <- createSiderealTargetAtAs(pi, pid, Far)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setExplicitBase(oid, Near)
      _   <- runObscalcUpdateAs(service, pid, oid)
      got <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, $NearCone""")
    yield assertEquals(got, List(oid))

  test("explicit base gives a position even with an opportunity target"):
    for
      pid <- createProgramAs(pi)
      tid <- createOpportunityTargetAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- setExplicitBase(oid, Near)
      _   <- runObscalcUpdateAs(service, pid, oid)
      got <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, $NearCone""")
    yield assertEquals(got, List(oid))

  private def createPmTargetAs(
    pid:        Program.Id,
    c:          Coordinates,
    epoch:      String,
    pmDecMasYr: Int
  ): IO[Target.Id] =
    query(
      pi,
      s"""mutation {
            createTarget(input: {
              programId: ${pid.asJson}
              SET: {
                name: "PM"
                sidereal: {
                  ${raInput(c)}
                  ${decInput(c)}
                  epoch: "$epoch"
                  properMotion: { ra: { milliarcsecondsPerYear: 0 }, dec: { milliarcsecondsPerYear: $pmDecMasYr } }
                  radialVelocity: { kilometersPerSecond: 0.0 }
                }
                $DefaultSourceProfile
              }
            }) { target { id } }
          }"""
    ).flatMap: js =>
      js.hcursor.downFields("createTarget", "target", "id").as[Target.Id]
        .leftMap(f => new RuntimeException(f.message))
        .liftTo[IO]

  // The stored position is PM-corrected to epoch J2000.0, not left at the catalog
  // epoch. A 10″/yr proper motion over the 16 years back from J2016 moves the
  // target ~160″, far beyond a 10″ cone, so a cone at the catalog position must
  // miss while a cone at the J2000 position (computed with lucuma-core, the same
  // math obscalc uses) must match.
  test("stored position is PM-corrected to J2000.0"):
    val catalog  = Near
    val tracking = SiderealTracking(
      catalog,
      Epoch.fromString.getOption("J2016.000").get,
      Some(ProperMotion(
        ProperMotion.RA.milliarcsecondsPerYear.reverseGet(BigDecimal(0)),
        ProperMotion.Dec.milliarcsecondsPerYear.reverseGet(BigDecimal(10000))
      )),
      none,
      none
    )
    val j2000Pos = tracking.at(Epoch.J2000.toInstant).get
    for
      pid  <- createProgramAs(pi)
      tid  <- createPmTargetAs(pid, catalog, "J2016.000", 10000)
      oid  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _    <- runObscalcUpdateAs(service, pid, oid)
      here <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, ${coneText(j2000Pos, 10L)}""")
      cat  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, ${coneText(catalog, 10L)}""")
    yield
      assertEquals(here, List(oid))
      assertEquals(cat, Nil)

  private def setTargetCoords(tid: Target.Id, c: Coordinates): IO[Unit] =
    query(
      pi,
      s"""mutation {
            updateTargets(input: {
              SET: { sidereal: { ${raInput(c)}, ${decInput(c)} } },
              WHERE: { id: { EQ: "$tid" } }
            }) { targets { id } }
          }"""
    ).void

  // The stored position is eventually consistent: it reflects the last obscalc
  // pass, not the transactional state, so an edit shows up only after the next
  // pass (in production, when the daemon catches up).
  test("position lags a target edit until obscalc runs again"):
    for
      pid    <- createProgramAs(pi)
      tid    <- createSiderealTargetAtAs(pi, pid, Near)
      oid    <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _      <- runObscalcUpdateAs(service, pid, oid)
      _      <- setTargetCoords(tid, Far)
      stale  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, $NearCone""")
      _      <- runObscalcUpdateAs(service, pid, oid)
      fresh  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, $NearCone""")
      moved  <- observationsWhere(pi, s"""program: { id: { EQ: "$pid" } }, ${coneText(Far, 18000L)}""")
    yield
      assertEquals(stale, List(oid))
      assertEquals(fresh, Nil)
      assertEquals(moved, List(oid))

  // The candidate prefilter is scoped to programs visible to the caller, and the
  // outer WHERE applies visibility as well.
  test("another PI sees nothing through the cone"):
    for
      (pid, _, _) <- coneSetup
      got <- observationsWhere(pi2, NearCone)
    yield assertEquals(got, Nil)

  private def expectConeRejected(query: String): IO[Unit] =
    expectOdbError(
      user = pi,
      query = query,
      expected = { case OdbError.InvalidArgument(Some(m)) if m.contains("targetCoordinates") => () }
    )

  // The update mutations share WhereObservation but keep their WHERE in an Env,
  // where the resolver cannot reach a cone, so the binding refuses it outright.
  test("cone is rejected by the update mutations"):
    for
      (pid, _, _) <- coneSetup
      _ <- expectConeRejected(
             s"""mutation {
                   updateObservations(input: {
                     SET: { subtitle: "x" },
                     WHERE: { program: { id: { EQ: "$pid" } }, $NearCone }
                   }) { observations { id } }
                 }"""
           )
      _ <- expectConeRejected(
             s"""mutation {
                   updateAsterisms(input: {
                     SET: { ADD: [] },
                     WHERE: { program: { id: { EQ: "$pid" } }, $NearCone }
                   }) { observations { id } }
                 }"""
           )
      _ <- expectConeRejected(
             s"""mutation {
                   updateObservationsTimes(input: {
                     SET: { observationTime: "2024-01-01 00:00:00" },
                     WHERE: { program: { id: { EQ: "$pid" } }, $NearCone }
                   }) { observations { id } }
                 }"""
           )
      _ <- expectConeRejected(
             s"""mutation {
                   updateDatasets(input: {
                     SET: { qaState: PASS },
                     WHERE: { observation: { $NearCone } }
                   }) { datasets { id } }
                 }"""
           )
    yield ()

  // Cones are refused under the group queries until tested there.
  test("cone is rejected by the group queries"):
    expectConeRejected(s"query { asterismGroup(WHERE: { $NearCone }) { matches { program { id } } } }")

  // WhereObservation is embedded in the dataset and execution-event WHEREs; the
  // cone must resolve there too (an unresolved placeholder fails loudly).
  test("cone reachable through the datasets and events WHEREs"):
    for
      (pid, _, _) <- coneSetup
      d <- query(pi, s"""query { datasets(WHERE: { observation: { $NearCone } }) { matches { id } } }""")
      e <- query(pi, s"""query { events(WHERE: { observation: { $NearCone } }) { matches { id } } }""")
    yield
      assertEquals(d.hcursor.downFields("datasets", "matches").values.toList.flatten, Nil)
      assertEquals(e.hcursor.downFields("events", "matches").values.toList.flatten, Nil)

}
