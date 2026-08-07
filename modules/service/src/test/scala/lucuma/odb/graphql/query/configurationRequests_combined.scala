// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.Order.catsKernelOrderingForOrder
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.Target

import java.time.LocalDate

// SC-9240 integration capstone: the destination query composes all four filters
// (status = APPROVED, program.isActive, observingModeType IN [...], targetCoordinates
// cone) and returns their exact intersection. The cone is injected as `id IN (...)`
// and the other filters are pushable, so the whole WHERE compiles as one SQL
// statement; LIMIT/OFFSET paginate cleanly over the exact result.
//
// Configuration requests are canonicalized by configuration, so each fixture
// request has a distinct (observing mode, target) configuration to keep their ids
// distinct; each non-matching one differs from the match in exactly one axis.
class configurationRequests_combined extends OdbSuite with ObservingModeSetupOperations {

  val pi    = TestUsers.Standard.pi(1, 30)
  val admin = TestUsers.Standard.admin(2, 31)
  val staff = TestUsers.Standard.staff(3, 32)
  val validUsers = List(pi, admin, staff)

  private def requestAs(pid: Program.Id, mode: ObservingModeType, tid: Target.Id): IO[ConfigurationRequest.Id] =
    for
      oid <- mode match
               case ObservingModeType.GmosSouthLongSlit => createGmosSouthLongSlitObservationAs(pi, pid, List(tid))
               case _                                   => createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      cid <- createConfigurationRequestAs(pi, oid)
    yield cid

  // The headline destination filter: approved, active program, GMOS-N long slit, within 5° of 00h +10°.
  private val Where =
    """status: { EQ: APPROVED }, program: { isActive: true }, observingModeType: { IN: [ GMOS_NORTH_LONG_SLIT ] }, targetCoordinates: { center: { ra: { hours: "0.0" }, dec: { degrees: "10.0" } }, distance: { arcseconds: 18000 } }"""

  private def destinationQuery(args: String): String =
    s"""
      query {
        configurationRequests(WHERE: { $Where }$args) {
          hasMore
          matches { id }
        }
      }
    """

  // Takes the `data` payload (what `query` yields; `queryWithSqlStats` yields a full response).
  private def idsAndHasMore(data: Json): IO[(List[ConfigurationRequest.Id], Boolean)] =
    val c = data.hcursor.downField("configurationRequests")
    (for
       ids     <- c.downField("matches").values.toList.flatten.traverse(_.hcursor.downField("id").as[ConfigurationRequest.Id])
       hasMore <- c.downField("hasMore").as[Boolean]
     yield (ids, hasMore))
      .leftMap(f => new RuntimeException(f.message))
      .liftTo[IO]

  private def page(args: String): IO[(List[ConfigurationRequest.Id], Boolean)] =
    query(pi, destinationQuery(args)).flatMap(idsAndHasMore)

  test("destination query: status + isActive + observingModeType + cone compose exactly"):
    val today = LocalDate.now()
    for
      cfpid <- createGeminiCallForProposalsAs(admin)

      pidActive   <- createProgramAs(pi)
      _           <- addProposal(pi, pidActive, Some(cfpid), None)
      _           <- setProgramActiveAs(staff, pidActive, today.minusDays(30), today.plusDays(30))

      pidInactive <- createProgramAs(pi)
      _           <- addProposal(pi, pidInactive, Some(cfpid), None)
      _           <- setProgramActiveAs(staff, pidInactive, today.plusDays(100), today.plusDays(200))

      // Distinct targets so each configuration request gets a distinct id.
      tAtCenter <- createSiderealTargetAtAs(pi, pidActive, "0.0",  "10.0")  // at the cone center
      tNear     <- createSiderealTargetAtAs(pi, pidActive, "0.1",  "10.0")  // ~1.5° off, still in the cone
      tNear2    <- createSiderealTargetAtAs(pi, pidActive, "0.2",  "10.0")  // ~3° off, still in the cone
      tFar      <- createSiderealTargetAtAs(pi, pidActive, "6.0",  "40.0")  // far outside the cone
      tInactive <- createSiderealTargetAtAs(pi, pidInactive, "0.0", "10.0") // inactive program

      // The matches: approved, GMOS-North-Long-Slit, active program, inside the cone.
      // Two of them, so LIMIT/OFFSET paging over the filtered set is meaningful.
      cidMatch     <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tAtCenter)
      _            <- setConfigurationRequestStatusAs(staff, cidMatch, ConfigurationRequestStatus.Approved)

      cidMatch2    <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tNear2)
      _            <- setConfigurationRequestStatusAs(staff, cidMatch2, ConfigurationRequestStatus.Approved)

      // Wrong observing mode (otherwise identical).
      cidWrongMode <- requestAs(pidActive, ObservingModeType.GmosSouthLongSlit, tAtCenter)
      _            <- setConfigurationRequestStatusAs(staff, cidWrongMode, ConfigurationRequestStatus.Approved)

      // Wrong status (right mode, in cone, active -- only status excludes it).
      cidWrongStatus <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tNear) // default Requested

      // Out of the cone (otherwise identical).
      cidOutOfCone <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tFar)
      _            <- setConfigurationRequestStatusAs(staff, cidOutOfCone, ConfigurationRequestStatus.Approved)

      // Inactive program (otherwise identical).
      cidInactiveProg <- requestAs(pidInactive, ObservingModeType.GmosNorthLongSlit, tInactive)
      _               <- setConfigurationRequestStatusAs(staff, cidInactiveProg, ConfigurationRequestStatus.Approved)

      expected = List(cidMatch, cidMatch2).sorted

      matched <- configurationRequestsWhere(pi, Where)

      // Pagination over the filtered set: OFFSET is inclusive, ordering is by id ascending.
      first  <- page(", LIMIT: 1")
      second <- page(s", OFFSET: ${expected(1).asJson}, LIMIT: 1")
      whole  <- page(", LIMIT: 1000")

      // Whitebox: the same query against a stats-collecting mapping, to count statements.
      (resp, stats) <- queryWithSqlStats(pi, destinationQuery(""))
      pushed        <- idsAndHasMore(resp.hcursor.downField("data").focus.getOrElse(Json.Null))
    yield
      // Only the approved, GMOS-North-Long-Slit, active-program requests in the cone match.
      assertEquals(matched.toSet, expected.toSet)

      assertEquals(first,  (List(expected(0)), true),  clue = "first page")
      assertEquals(second, (List(expected(1)), false), clue = "second page")
      assertEquals(whole,  (expected, false),          clue = "unpaginated")

      // The cone is injected as `id IN (...)`, so status / isActive / observingModeType /
      // id / visibility all push down into a single statement -- no per-row Scala
      // evaluation. (The cone candidate lookup itself runs before compilation and is not
      // counted here; see `queryWithSqlStats`.)
      assertEquals(pushed._1.toSet, expected.toSet)
      assertEquals(stats.length, 1, clue = stats.map(_.normalize.sql))

}
