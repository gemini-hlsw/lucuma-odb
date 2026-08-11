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

// The destination query: all four filters in one WHERE, returning their exact intersection.
//
// The fixture is built one-axis-off -- two matches, then a decoy per filter that differs from
// a match in that filter alone -- so whichever filter breaks, the decoy that leaks names it.
//
// Requests are canonicalized by configuration, so each needs a distinct (mode, target) pair
// to get a distinct id.
class configurationRequests_combined extends OdbSuite with ObservingModeSetupOperations with ConeSearchFixture:

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

  // Approved, active program, GMOS-N long slit, within 5° (18000") of (0h, +10°).
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

  // Takes the `data` payload, which is what `query` yields; `queryWithSqlStats` yields the
  // whole response, so its caller has to unwrap `data` first.
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

  test("destination query: status + isActive + observingModeType + cone compose"):
    val today = LocalDate.now()
    for
      cfpid <- createGeminiCallForProposalsAs(admin)

      pidActive   <- createProgramAs(pi)
      _           <- addProposal(pi, pidActive, Some(cfpid), None)
      _           <- setProgramActiveAs(staff, pidActive, today.minusDays(30), today.plusDays(30))

      pidInactive <- createProgramAs(pi)
      _           <- addProposal(pi, pidInactive, Some(cfpid), None)
      _           <- setProgramActiveAs(staff, pidInactive, today.plusDays(100), today.plusDays(200))

      tAtCenter <- createSiderealTargetAtAs(pi, pidActive, coords("00:00:00 +10:00:00"))   // cone center
      tNear     <- createSiderealTargetAtAs(pi, pidActive, coords("00:06:00 +10:00:00"))   // ~1.5°, in the cone
      tNear2    <- createSiderealTargetAtAs(pi, pidActive, coords("00:12:00 +10:00:00"))   // ~3°, in the cone
      tFar      <- createSiderealTargetAtAs(pi, pidActive, coords("06:00:00 +40:00:00"))   // outside the cone
      tInactive <- createSiderealTargetAtAs(pi, pidInactive, coords("00:00:00 +10:00:00")) // inactive program

      // Two matches, so paging over the filtered set below is meaningful.
      cidMatch     <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tAtCenter)
      _            <- setConfigurationRequestStatusAs(staff, cidMatch, ConfigurationRequestStatus.Approved)

      cidMatch2    <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tNear2)
      _            <- setConfigurationRequestStatusAs(staff, cidMatch2, ConfigurationRequestStatus.Approved)

      // The decoys. Each is a match except for the one filter named.

      cidWrongMode <- requestAs(pidActive, ObservingModeType.GmosSouthLongSlit, tAtCenter)
      _            <- setConfigurationRequestStatusAs(staff, cidWrongMode, ConfigurationRequestStatus.Approved)

      cidWrongStatus <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tNear) // left Requested

      cidOutOfCone <- requestAs(pidActive, ObservingModeType.GmosNorthLongSlit, tFar)
      _            <- setConfigurationRequestStatusAs(staff, cidOutOfCone, ConfigurationRequestStatus.Approved)

      cidInactiveProg <- requestAs(pidInactive, ObservingModeType.GmosNorthLongSlit, tInactive)
      _               <- setConfigurationRequestStatusAs(staff, cidInactiveProg, ConfigurationRequestStatus.Approved)

      expected = List(cidMatch, cidMatch2).sorted

      matched <- configurationRequestsWhere(pi, Where)

      // Paging runs over the filtered set, which only holds if the cone is part of the WHERE.
      // OFFSET is inclusive and matches are ordered by id.
      first  <- page(", LIMIT: 1")
      second <- page(s", OFFSET: ${expected(1).asJson}, LIMIT: 1")
      whole  <- page(", LIMIT: 1000")

      // The same query again, through a mapping that records the SQL it issues.
      (resp, stats) <- queryWithSqlStats(pi, destinationQuery(""))
      pushed        <- idsAndHasMore(resp.hcursor.downField("data").focus.getOrElse(Json.Null))
    yield
      assertEquals(matched.toSet, expected.toSet)

      assertEquals(first,  (List(expected(0)), true),  clue = "first page")
      assertEquals(second, (List(expected(1)), false), clue = "second page")
      assertEquals(whole,  (expected, false),          clue = "unpaginated")

      // Once the cone becomes `id IN (...)` every filter is pushable, so the whole query is
      // one statement with nothing evaluated per row in Scala.
      assertEquals(pushed._1.toSet, expected.toSet)
      // Verify we use a single SQL statement
      assertEquals(stats.length, 1, clue = stats.map(_.normalize.sql))
