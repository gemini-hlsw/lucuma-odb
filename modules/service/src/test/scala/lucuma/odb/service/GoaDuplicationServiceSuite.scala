// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.goa.GoaObservationClass
import lucuma.catalog.goa.GoaObservationType
import lucuma.catalog.goa.GoaSummaryRecord
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.Timestamp
import lucuma.odb.data.GoaDuplication
import lucuma.odb.data.GoaSearchCenter
import lucuma.odb.graphql.OdbSuite
import lucuma.odb.graphql.TestUsers
import skunk.Transaction

import java.time.Instant
import java.time.LocalDate

class GoaDuplicationServiceSuite extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(1, 30)

  override val validUsers: List[User] = List(pi)

  private val searchCenter: Coordinates =
    Coordinates.fromHmsDms.unsafeGet("17 57 48.49803 +04 41 36.2072")

  private val checkedAt: Timestamp =
    Timestamp.unsafeFromInstantTruncated(Instant.parse("2026-07-21T00:00:00Z"))

  /** Every optional column populated, to catch a mis-ordered codec. */
  private val fullRecord: GoaSummaryRecord =
    GoaSummaryRecord(
      name             = "N20190101S0123.fits",
      dataLabel        = "GN-2019A-Q-101-1-001".some,
      ra               = searchCenter.ra.some,
      dec              = searchCenter.dec.some,
      instrument       = "GMOS-N",
      observationType  = GoaObservationType.Object,
      observationClass = GoaObservationClass.Science.some,
      qaState          = "Pass".some,
      utDateTime       = Instant.parse("2019-01-01T09:08:07Z").some,
      releaseDate      = LocalDate.of(2020, 7, 1).some,
      programId        = "GN-2019A-Q-101".some,
      observationId    = "GN-2019A-Q-101-1".some,
      objectName       = "M51".some,
      exposure         = 300.secondTimeSpan.some,
      disperser        = "R400+_G5305".some,
      filter           = "i_G0302".some,
      wavelength       = Wavelength.fromIntNanometers(700),
      airmass          = 1.234.some,
      azimuth          = Angle.fromDoubleDegrees(123.5).some,
      elevation        = Angle.fromDoubleDegrees(67.25).some
    )

  /** Nothing but the required columns, to catch a null-handling mistake. */
  private val sparseRecord: GoaSummaryRecord =
    GoaSummaryRecord(
      name             = "S20200202S0456.fits",
      dataLabel        = none,
      ra               = none,
      dec              = none,
      instrument       = "GMOS-S",
      observationType  = GoaObservationType.Unknown("WEIRD"),
      observationClass = none,
      qaState          = none,
      utDateTime       = none,
      releaseDate      = none,
      programId        = none,
      observationId    = none,
      objectName       = none,
      exposure         = none,
      disperser        = none,
      filter           = none,
      wavelength       = none,
      airmass          = none,
      azimuth          = none,
      elevation        = none
    )

  private def header(count: Int, center: Option[GoaSearchCenter]): GoaDuplication.Header =
    GoaDuplication.Header(
      GoaDuplication.State.Checked,
      NonNegInt.unsafeFrom(count),
      saturated     = false,
      lastCheckedAt = checkedAt.some,
      error         = none,
      provenance    = GoaDuplication.Provenance(center, Angle.fromDoubleArcseconds(180.0).some)
    )

  private def sidereal(count: Int): GoaDuplication.Header =
    header(count, GoaSearchCenter.Sidereal(searchCenter).some)

  private def newObservation: IO[Observation.Id] =
    createProgramAs(pi).flatMap(createObservationAs(pi, _))

  private def run[A](f: Transaction[IO] ?=> GoaDuplicationService[IO] => IO[A]): IO[A] =
    withServices(pi): services =>
      services.transactionally:
        f(services.goaDuplicationService)

  test("an observation that has never been searched reads as not checked"):
    for
      oid <- newObservation
      s   <- run(_.select(oid))
    yield
      assertEquals(s.header, GoaDuplication.Header.NeverChecked)
      assertEquals(s.matches, Nil)

  test("a stored snapshot round-trips with every column"):
    val h  = sidereal(2)
    val ms = List(fullRecord, sparseRecord)
    for
      oid <- newObservation
      _   <- run(_.store(oid, h, ms))
      s   <- run(_.select(oid))
    yield
      assertEquals(s.header, h)
      assertEquals(s.matches.sortBy(_.name), ms.sortBy(_.name))

  test("a non-sidereal search center round-trips as a target name"):
    val name = NonEmptyString.unsafeFrom("Ceres")
    val h    = header(0, GoaSearchCenter.NonSidereal(name).some)
    for
      oid <- newObservation
      _   <- run(_.store(oid, h, Nil))
      s   <- run(_.select(oid))
    yield assertEquals(s.header.provenance.center, GoaSearchCenter.NonSidereal(name).some)

  test("storing replaces the previous snapshot rather than appending to it"):
    for
      oid <- newObservation
      _   <- run(_.store(oid, sidereal(2), List(fullRecord, sparseRecord)))
      _   <- run(_.store(oid, sidereal(1), List(sparseRecord)))
      s   <- run(_.select(oid))
    yield
      assertEquals(s.header.matchCount.value, 1)
      assertEquals(s.matches, List(sparseRecord))

  test("an error leaves the previous matches and last checked time intact"):
    for
      oid <- newObservation
      _   <- run(_.store(oid, sidereal(1), List(fullRecord)))
      _   <- run(_.storeError(oid, "GOA is down"))
      s   <- run(_.select(oid))
    yield
      assertEquals(s.header.state, GoaDuplication.State.Error)
      assertEquals(s.header.error, "GOA is down".some)
      assertEquals(s.header.matchCount.value, 1)
      assertEquals(s.header.lastCheckedAt, checkedAt.some)
      assertEquals(s.matches, List(fullRecord))

  test("an error with no previous snapshot reports the error and no matches"):
    for
      oid <- newObservation
      _   <- run(_.storeError(oid, "GOA is down"))
      s   <- run(_.select(oid))
    yield
      assertEquals(s.header.state, GoaDuplication.State.Error)
      assertEquals(s.header.lastCheckedAt, none)
      assertEquals(s.header.matchCount.value, 0)
      assertEquals(s.matches, Nil)

  test("a not-checked snapshot records that the search ran but could not be performed"):
    val h = GoaDuplication.Header.notChecked(checkedAt, GoaDuplication.Provenance.Empty)
    for
      oid <- newObservation
      _   <- run(_.store(oid, h, Nil))
      s   <- run(_.select(oid))
    yield
      assertEquals(s.header.state, GoaDuplication.State.NotChecked)
      assertEquals(s.header.lastCheckedAt, checkedAt.some)
      assertEquals(s.matches, Nil)
