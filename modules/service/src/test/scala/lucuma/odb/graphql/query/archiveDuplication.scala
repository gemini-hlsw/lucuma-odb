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
import lucuma.odb.data.ArchiveSearchPointing
import lucuma.odb.service.ArchiveDuplicationSearchService
import lucuma.odb.service.Services
import org.typelevel.otel4s.trace.Tracer.Implicits.noop

class archiveDuplication extends OdbSuite:

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

  /**
   * The archive is an OCS-era system whose vocabularies are open: timestamps
   * carry fractional seconds and sometimes an offset, and classes and types
   * appear that lucuma-core has no case for.  None of it should be lost.
   */
  private val AwkwardRecord: String =
    """
      [
        {
          "name": "S20240101S0002.fits",
          "instrument": "GMOS-S",
          "observation_type": "TELLURIC_STANDARD",
          "observation_class": "science_verification",
          "qa_state": "Undefined",
          "ut_datetime": "2024-01-01 03:04:05.678",
          "release": "2025-07-01"
        },
        {
          "name": "S20240101S0003.fits",
          "instrument": "GMOS-S",
          "observation_type": "OBJECT",
          "ut_datetime": "2024-01-01 03:04:05-05:00"
        }
      ]
    """

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
      ArchiveDuplicationSearchService.instantiate(client).refresh(oid).flatMap(_.get).void

  private def searchPointingOf(oid: Observation.Id): IO[Option[Coordinates]] =
    withServices(pi): services =>
      services
        .transactionally(services.archiveDuplicationService.selectSummary(oid))
        .map(_.searchArea.center.collect { case ArchiveSearchPointing.Sidereal(c) => c })

  private def archiveDuplication(oid: Observation.Id, fields: String): IO[Json] =
    query(
      user  = pi,
      query = s"""
        query {
          observation(observationId: ${oid.asJson}) {
            archiveDuplication { $fields }
          }
        }
      """
    ).map(_.hcursor.downField("observation").downField("archiveDuplication").focus.get)

  test("an observation that has never been searched reads as not checked"):
    for
      oid <- siderealObservation
      js  <- archiveDuplication(oid, "state matchCount saturated lastCheckedAt error matches { name }")
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
      js  <- archiveDuplication(oid, """
               state
               matchCount
               saturated
               error
               matches {
                 name
                 dataLabel
                 coordinates { ra { degrees } dec { degrees } }
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
              "coordinates": { "ra": { "degrees": 0.0 }, "dec": { "degrees": 0.01 } },
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

  test("vocabularies GOA knows and lucuma-core does not survive the round trip"):
    for
      oid <- siderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](AwkwardRecord))(oid)
      js  <- archiveDuplication(oid, "matchCount matches { name observationType observationClass qaState utDateTime }")
    yield assertEquals(
      js,
      json"""
        {
          "matchCount": 2,
          "matches": [
            {
              "name": "S20240101S0002.fits",
              "observationType": "TELLURIC_STANDARD",
              "observationClass": "science_verification",
              "qaState": "Undefined",
              "utDateTime": "2024-01-01T03:04:05.678Z"
            },
            {
              "name": "S20240101S0003.fits",
              "observationType": "OBJECT",
              "observationClass": null,
              "qaState": null,
              "utDateTime": "2024-01-01T08:04:05Z"
            }
          ]
        }
      """
    )

  test("the search area is reported alongside the matches"):
    for
      oid    <- siderealObservation
      _      <- refresh(GoaClientMock.fromJson[IO](SparseRecord))(oid)
      center <- searchPointingOf(oid)
      js     <- archiveDuplication(oid, """
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

  test("distance is the separation from the stored search center"):
    for
      oid    <- siderealObservation
      _      <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      center <- searchPointingOf(oid)
      js     <- archiveDuplication(oid, "matches { coordinates { ra { degrees } dec { degrees } } distance { microarcseconds } }")
    yield
      val m        = js.hcursor.downField("matches").downN(0)
      val coords   = m.downField("coordinates")
      val ra       = coords.downField("ra").downField("degrees").as[BigDecimal].toOption.get
      val dec      = coords.downField("dec").downField("degrees").as[BigDecimal].toOption.get
      val actual   = m.downField("distance").downField("microarcseconds").as[Long].toOption.get
      val matchAt  = Coordinates(
                       RightAscension.fromDoubleDegrees(ra.toDouble),
                       Declination.fromDoubleDegrees(dec.toDouble).get
                     )
      assertEquals(actual, center.get.angularDistance(matchAt).toMicroarcseconds)

  test("distance is null for a match with no pointing"):
    for
      oid <- siderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](SparseRecord))(oid)
      js  <- archiveDuplication(oid, "matches { name coordinates { ra { degrees } } distance { microarcseconds } }")
    yield assertEquals(
      js,
      json"""
        {
          "matches": [
            { "name": "a.fits", "coordinates": null, "distance": null }
          ]
        }
      """
    )

  test("a non-sidereal search reports the target name and no distances"):
    for
      oid <- nonsiderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      js  <- archiveDuplication(oid, """
               searchTargetName
               searchCoordinates { ra { degrees } }
               matches { name distance { microarcseconds } }
             """)
    yield assertEquals(
      js,
      json"""
        {
          "searchTargetName": "Halley",
          "searchCoordinates": null,
          "matches": [
            { "name": "S20240101S0001.fits", "distance": null }
          ]
        }
      """
    )

  test("re-checking replaces the previous matches"):
    for
      oid <- siderealObservation
      _   <- refresh(GoaClientMock.fromJson[IO](FullRecord))(oid)
      _   <- refresh(GoaClientMock.fromJson[IO](SparseRecord))(oid)
      js  <- archiveDuplication(oid, "matchCount matches { name }")
    yield assertEquals(
      js,
      json"""{ "matchCount": 1, "matches": [ { "name": "a.fits" } ] }"""
    )
