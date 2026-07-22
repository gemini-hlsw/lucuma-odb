// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.all.*
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.goa.GoaClientMock
import lucuma.core.enums.VisitorObservingModeType
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.GoaDuplication
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers

class GoaDuplicationSearchServiceSuite extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(1, 30)

  override val validUsers: List[User] = List(pi)

  /**
   * GOA's summary records carry only `name`, `instrument` and
   * `observation_type` as required fields, which is all these tests need to
   * distinguish and count files.
   */
  private def responseOf(names: String*): String =
    names
      .map: n =>
        s"""{"name": "$n", "instrument": "GMOS-N", "observation_type": "OBJECT"}"""
      .mkString("[", ", ", "]")

  private def mockOf(names: String*): GoaClient[IO] =
    GoaClientMock.fromJson[IO](responseOf(names*))

  /** Anything GOA cannot parse fails the query the way an outage would. */
  private val brokenMock: GoaClient[IO] =
    GoaClientMock.fromJson[IO]("this is not JSON")

  private def gmosObservation: IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
    yield oid

  private def maroonXObservation: IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createVisitorModeObservationAs(pi, pid, VisitorObservingModeType.MaroonX, tid)
    yield oid

  private def refresh(client: GoaClient[IO])(oid: Observation.Id): IO[GoaDuplication.Snapshot] =
    withServices(pi): services =>
      given Services[IO] = services
      GoaDuplicationSearchService.instantiate(client).refresh(oid).flatMap(_.get)

  /** What is actually in the database, as opposed to what `refresh` returned. */
  private def stored(oid: Observation.Id): IO[GoaDuplication.Snapshot] =
    withServices(pi): services =>
      services.transactionally(services.goaDuplicationService.select(oid))

  test("matches are counted per file and persisted"):
    for
      oid <- gmosObservation
      s   <- refresh(mockOf("a.fits", "b.fits", "c.fits"))(oid)
      db  <- stored(oid)
    yield
      assertEquals(s.header.state, GoaDuplication.State.Checked)
      assertEquals(s.header.matchCount.value, 3)
      assertEquals(db.matches.map(_.name), List("a.fits", "b.fits", "c.fits"))

  test("a file returned by both queries in the group is counted once"):
    // GMOS fans out to GMOS-N and GMOS-S, and the mock answers both alike, so
    // every record arrives twice.
    for
      oid <- gmosObservation
      s   <- refresh(mockOf("a.fits", "b.fits"))(oid)
      db  <- stored(oid)
    yield
      assertEquals(s.header.matchCount.value, 2)
      assertEquals(db.matches.map(_.name), List("a.fits", "b.fits"))

  test("a search records where and how wide it looked"):
    for
      oid <- gmosObservation
      s   <- refresh(mockOf("a.fits"))(oid)
    yield
      assert(s.header.searchArea.center.isDefined)
      assert(s.header.searchArea.radius.isDefined)
      assert(s.header.lastCheckedAt.isDefined)

  test("no matches is a successful search, not an error"):
    for
      oid <- gmosObservation
      s   <- refresh(GoaClientMock.empty[IO])(oid)
    yield
      assertEquals(s.header.state, GoaDuplication.State.Checked)
      assertEquals(s.header.matchCount.value, 0)
      assertEquals(s.header.error, none)
      assertEquals(s.matches, Nil)

  test("a query filled to GOA's cap is saturated"):
    val names = (1 to GoaDuplication.QueryLimit).toList.map(i => s"f$i.fits")
    for
      oid <- gmosObservation
      s   <- refresh(mockOf(names*))(oid)
    yield
      assertEquals(s.header.matchCount.value, GoaDuplication.QueryLimit)
      assert(s.header.saturated)

  test("a query short of the cap is not saturated"):
    val names = (1 until GoaDuplication.QueryLimit).toList.map(i => s"f$i.fits")
    for
      oid <- gmosObservation
      s   <- refresh(mockOf(names*))(oid)
    yield
      assertEquals(s.header.matchCount.value, GoaDuplication.QueryLimit - 1)
      assert(!s.header.saturated)

  test("refreshing replaces the previous snapshot"):
    for
      oid <- gmosObservation
      _   <- refresh(mockOf("a.fits", "b.fits", "c.fits"))(oid)
      _   <- refresh(mockOf("d.fits"))(oid)
      db  <- stored(oid)
    yield
      assertEquals(db.header.matchCount.value, 1)
      assertEquals(db.matches.map(_.name), List("d.fits"))

  test("a GOA failure is reported without destroying the last good snapshot"):
    for
      oid <- gmosObservation
      _   <- refresh(mockOf("a.fits", "b.fits"))(oid)
      s   <- refresh(brokenMock)(oid)
      db  <- stored(oid)
    yield
      assertEquals(s.header.state, GoaDuplication.State.Error)
      assert(s.header.error.isDefined)
      assertEquals(db.header.matchCount.value, 2)
      assertEquals(db.matches.map(_.name), List("a.fits", "b.fits"))

  test("a GOA failure with no previous snapshot is still not a failed call"):
    for
      oid <- gmosObservation
      s   <- refresh(brokenMock)(oid)
    yield
      assertEquals(s.header.state, GoaDuplication.State.Error)
      assertEquals(s.header.matchCount.value, 0)
      assertEquals(s.matches, Nil)

  test("an instrument GOA does not know is reported as not checked"):
    for
      oid <- maroonXObservation
      s   <- refresh(mockOf("a.fits"))(oid)
      db  <- stored(oid)
    yield
      assertEquals(s.header.state, GoaDuplication.State.NotChecked)
      assertEquals(s.header.error, none)
      assertEquals(s.matches, Nil)
      assertEquals(db.header.state, GoaDuplication.State.NotChecked)
      assert(db.header.lastCheckedAt.isDefined)

  test("an observation with no pointing is reported as not checked"):
    for
      pid <- createProgramAs(pi)
      oid <- createGmosNorthImagingObservationAs(pi, pid)
      s   <- refresh(mockOf("a.fits"))(oid)
    yield
      assertEquals(s.header.state, GoaDuplication.State.NotChecked)
      assertEquals(s.header.searchArea.center, none)
      assertEquals(s.matches, Nil)
