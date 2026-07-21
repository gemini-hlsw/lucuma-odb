// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.goa.GoaClientMock
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.odb.data.GoaSearchCenter
import lucuma.odb.service.GoaDuplicationSearchService
import lucuma.odb.service.Services

class goaDuplication extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(1, 30)

  override val validUsers: List[User] = List(pi)

  /** Every field GOA reports, so the mapping is exercised end to end. */
  private val FullRecord: String =
    """
      [
        {
          "name": "S20240101S0001.fits",
          "data_label": "GS-2024A-Q-1-1-001",
          "ra": 0.0,
          "dec": 0.01,
          "instrument": "GMOS-S",
          "observation_type": "OBJECT",
          "observation_class": "science",
          "qa_state": "Pass",
          "ut_datetime": "2024-01-01 03:04:05",
          "release": "2025-07-01",
          "program_id": "GS-2024A-Q-1",
          "observation_id": "GS-2024A-Q-1-1",
          "object": "NGC 1234",
          "exposure_time": 300.0,
          "disperser": "R400_G5325",
          "filter_name": "r_G0326",
          "central_wavelength": 0.7,
          "airmass": 1.234,
          "azimuth": 12.5,
          "elevation": 67.5
        }
      ]
    """

  /** GOA reports only name, instrument and observation type as required. */
  private val SparseRecord: String =
    """[{ "name": "a.fits", "instrument": "GMOS-N", "observation_type": "OBJECT" }]"""

  private def siderealObservation: IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid)
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
    yield oid

  private def nonsiderealObservation: IO[Observation.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid, name = "Halley")
      oid <- createGmosNorthImagingObservationAs(pi, pid, tid)
    yield oid

  private def refresh(client: GoaClient[IO])(oid: Observation.Id): IO[Unit] =
    withServices(pi): services =>
      given Services[IO] = services
      GoaDuplicationSearchService.instantiate(client).refresh(oid).flatMap(_.get).void

  private def searchCenterOf(oid: Observation.Id): IO[Option[Coordinates]] =
    withServices(pi): services =>
      services
        .transactionally(services.goaDuplicationService.selectHeader(oid))
        .map(_.provenance.center.collect { case GoaSearchCenter.Sidereal(c) => c })

  private def goaDuplication(oid: Observation.Id, fields: String): IO[Json] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: ${oid.asJson}) {
            goaDuplication { $fields }
          }
        }
      """
    ).map(_.hcursor.downField("observation").downField("goaDuplication").focus.get)

  test("an observation that has never been searched reads as not checked"):
    for
      oid <- siderealObservation
      js  <- goaDuplication(oid, "state matchCount saturated lastCheckedAt error matches { name }")
    yield assertEquals(
      js,
      json"""
        {
          "state": "NOT_CHECKED",
          "matchCount": 0,
          "saturated": false,
          "lastCheckedAt": null,
          "error": null,
          "matches": []
        }
      """
    )

  test("a refreshed search reports every stored column"):
    for
      oid <- siderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      js  <- goaDuplication(oid, """
               state
               matchCount
               saturated
               error
               matches {
                 name
                 dataLabel
                 ra { degrees }
                 dec { degrees }
                 instrument
                 observationType
                 observationClass
                 qaState
                 utDateTime
                 releaseDate
                 programId
                 observationId
                 objectName
                 exposure { seconds }
                 disperser
                 filter
                 wavelength { nanometers }
                 airmass
                 azimuth { degrees }
                 elevation { degrees }
               }
             """)
    yield assertEquals(
      js,
      json"""
        {
          "state": "CHECKED",
          "matchCount": 1,
          "saturated": false,
          "error": null,
          "matches": [
            {
              "name": "S20240101S0001.fits",
              "dataLabel": "GS-2024A-Q-1-1-001",
              "ra": { "degrees": 0.0 },
              "dec": { "degrees": 0.01 },
              "instrument": "GMOS-S",
              "observationType": "OBJECT",
              "observationClass": "science",
              "qaState": "Pass",
              "utDateTime": "2024-01-01T03:04:05Z",
              "releaseDate": "2025-07-01",
              "programId": "GS-2024A-Q-1",
              "observationId": "GS-2024A-Q-1-1",
              "objectName": "NGC 1234",
              "exposure": { "seconds": 300.000000 },
              "disperser": "R400_G5325",
              "filter": "r_G0326",
              "wavelength": { "nanometers": 700.000 },
              "airmass": 1.234,
              "azimuth": { "degrees": 12.5 },
              "elevation": { "degrees": 67.5 }
            }
          ]
        }
      """
    )

  test("the search provenance is reported alongside the matches"):
    for
      oid    <- siderealObservation
      _      <- refresh(GoaClientMock.fromJson[IO](SparseRecord))(oid)
      center <- searchCenterOf(oid)
      js     <- goaDuplication(oid, """
                  lastCheckedAt
                  searchTargetName
                  searchCoordinates { ra { microseconds } dec { microarcseconds } }
                  searchRadius { microarcseconds }
                """)
    yield
      val c = center.get
      assertEquals(
        js.hcursor.downField("searchCoordinates").focus,
        json"""
          {
            "ra":  { "microseconds": ${c.ra.toHourAngle.toMicroseconds} },
            "dec": { "microarcseconds": ${c.dec.toAngle.toMicroarcseconds} }
          }
        """.some
      )
      assertEquals(js.hcursor.downField("searchTargetName").focus, Json.Null.some)
      assert(js.hcursor.downField("lastCheckedAt").focus.exists(!_.isNull))
      assert(js.hcursor.downField("searchRadius").downField("microarcseconds").focus.exists(!_.isNull))

  test("distanceArcsec is the separation from the stored search center"):
    for
      oid    <- siderealObservation
      _      <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      center <- searchCenterOf(oid)
      js     <- goaDuplication(oid, "matches { ra { degrees } dec { degrees } distanceArcsec }")
    yield
      val m        = js.hcursor.downField("matches").downN(0)
      val ra       = m.downField("ra").downField("degrees").as[BigDecimal].toOption.get
      val dec      = m.downField("dec").downField("degrees").as[BigDecimal].toOption.get
      val actual   = m.downField("distanceArcsec").as[BigDecimal].toOption.get
      val matchAt  = Coordinates(
                       RightAscension.fromDoubleDegrees(ra.toDouble),
                       Declination.fromDoubleDegrees(dec.toDouble).get
                     )
      val expected = BigDecimal(center.get.angularDistance(matchAt).toMicroarcseconds) / 1_000_000
      assertEquals(actual, expected)

  test("distanceArcsec is null for a match with no pointing"):
    for
      oid <- siderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](SparseRecord))(oid)
      js  <- goaDuplication(oid, "matches { name ra { degrees } dec { degrees } distanceArcsec }")
    yield assertEquals(
      js,
      json"""
        {
          "matches": [
            { "name": "a.fits", "ra": null, "dec": null, "distanceArcsec": null }
          ]
        }
      """
    )

  test("a non-sidereal search reports the target name and no distances"):
    for
      oid <- nonsiderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      js  <- goaDuplication(oid, """
               searchTargetName
               searchCoordinates { ra { degrees } }
               matches { name distanceArcsec }
             """)
    yield assertEquals(
      js,
      json"""
        {
          "searchTargetName": "Halley",
          "searchCoordinates": null,
          "matches": [
            { "name": "S20240101S0001.fits", "distanceArcsec": null }
          ]
        }
      """
    )

  test("re-checking replaces the previous matches"):
    for
      oid <- siderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      _   <- refresh(GoaClientMock.fromJson[IO](SparseRecord))(oid)
      js  <- goaDuplication(oid, "matchCount matches { name }")
    yield assertEquals(
      js,
      json"""{ "matchCount": 1, "matches": [ { "name": "a.fits" } ] }"""
    )
