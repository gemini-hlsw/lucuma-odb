// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

import java.time.LocalDateTime

class basePosition extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  val when = LocalDateTime.of(2026, 5, 20, 12, 0, 0)

  private val obsTime: Timestamp = Timestamp.fromLocalDateTime(when).get

  private val duration: TimeSpan = 1.hourTimeSpan

  private val explicitBaseUpdate = """
    targetEnvironment: {
      explicitBase: {
        ra:  { hms: "1:00:00" },
        dec: { dms: "2:00:00" }
      }
    }
  """

  private def query(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: ${oid.asJson}) {
          targetEnvironment {
            basePosition {
              type
              name
              sidereal {
                ra  { hours }
                dec { degrees }
                epoch
                properMotion {
                  ra  { milliarcsecondsPerYear }
                  dec { milliarcsecondsPerYear }
                }
                radialVelocity { kilometersPerSecond }
                parallax       { milliarcseconds }
              }
              nonsidereal { keyType des key }
              coordinates { ra { hours } dec { degrees } }
            }
          }
        }
      }
    """

  private def updateObservation(user: User, oid: Observation.Id, set: String): IO[Unit] =
    expect(
      user,
      s"""
        mutation {
          updateObservations(input: {
            SET: { $set }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) { observations { id } }
        }
      """,
      json"""
        {
          "updateObservations": {
            "observations": [
              { "id": ${oid.asJson} }
            ]
          }
        }
      """.asRight
    )

  test("[baseposition] single sidereal target"):
    val createWithTracking: Program.Id => String = pid => s"""
      mutation {
        createTarget(input: {
          programId: ${pid.asJson}
          SET: {
            name: "M999"
            sidereal: {
              ra: { hours: "1.0" }
              dec: { degrees: "2.0" }
              epoch: "J2000.000"
              properMotion: {
                ra:  { milliarcsecondsPerYear: "10.5" }
                dec: { milliarcsecondsPerYear: "-5.25" }
              }
              radialVelocity: { kilometersPerSecond: "42.0" }
              parallax: { milliarcseconds: "7.5" }
            }
            sourceProfile: {
              point: {
                bandNormalized: {
                  sed: { stellarLibrary: B5_III }
                  brightnesses: []
                }
              }
            }
          }
        }) { target { id } }
      }
    """

    for
      pid <- createProgramAs(pi)
      tid <- query(pi, createWithTracking(pid)).map { js =>
               js.hcursor
                 .downFields("createTarget", "target", "id")
                 .require[Target.Id]
             }
      oid <- createObservationAs(pi, pid, tid)
      _   <- expect(pi, query(oid),
        json"""
          {
            "observation": {
              "targetEnvironment": {
                "basePosition": {
                  "type": "SINGLE_TARGET",
                  "name": "M999",
                  "sidereal": {
                    "ra":  { "hours": 1.000000 },
                    "dec": { "degrees": 2.0 },
                    "epoch": "J2000.000",
                    "properMotion": {
                      "ra":  { "milliarcsecondsPerYear": 10.500 },
                      "dec": { "milliarcsecondsPerYear": -5.250 }
                    },
                    "radialVelocity": { "kilometersPerSecond": 42.0000 },
                    "parallax": { "milliarcseconds":    7.5 }
                  },
                  "nonsidereal": null,
                  "coordinates": null
                }
              }
            }
          }
        """.asRight)
    yield ()

  test("[baseposition] single nonsidereal target"):
    for
      pid <- createProgramAs(pi)
      tid <- createNonsiderealTargetAs(pi, pid, "Halley")
      oid <- createObservationAs(pi, pid, tid)
      _   <- expect(pi, query(oid),
        json"""
          {
            "observation": {
              "targetEnvironment": {
                "basePosition": {
                  "type": "SINGLE_TARGET",
                  "name": "Halley",
                  "sidereal": null,
                  "nonsidereal": {
                    "keyType": "COMET",
                    "des": "1P",
                    "key": "Comet_1P"
                  },
                  "coordinates": null
                }
              }
            }
          }
        """.asRight)
    yield ()

  test("[baseposition] single opportunity target"):
    for
      pid <- createProgramAs(pi)
      tid <- createOpportunityTargetAs(pi, pid, "ToO")
      oid <- createObservationAs(pi, pid, tid)
      _   <- expect(pi, query(oid),
        json"""
          {
            "observation": {
              "targetEnvironment": {
                "basePosition": null
              }
            }
          }
        """.asRight)
    yield ()

  test("[baseposition] single target with explicit base"):
    for
      pid <- createProgramAs(pi)
      tid <- createTargetAs(pi, pid, "M999")
      oid <- createObservationAs(pi, pid, tid)
      _   <- updateObservation(pi, oid, explicitBaseUpdate)
      _   <- expect(pi, query(oid),
        json"""
          {
            "observation": {
              "targetEnvironment": {
                "basePosition": {
                  "type": "EXPLICIT_BASE",
                  "name": "M999",
                  "sidereal": null,
                  "nonsidereal": null,
                  "coordinates": {
                    "ra":  { "hours": 1.000000 },
                    "dec": { "degrees": 2.0 }
                  }
                }
              }
            }
          }
        """.asRight)
    yield ()

  test("[baseposition] center coords of the asterism and obs time"):
    def setCoords(tid: Target.Id, raDeg: String, decDeg: String): IO[Unit] =
      query(
        pi,
        s"""
          mutation {
            updateTargets(input: {
              SET: {
                sidereal: {
                  ra:  { degrees: "$raDeg" }
                  dec: { degrees: "$decDeg" }
                }
              }
              WHERE: { id: { EQ: "$tid" } }
            }) { targets { id } }
          }
        """
      ).void

    val basePosQuery: Observation.Id => String = oid => s"""
      query {
        observation(observationId: ${oid.asJson}) {
          targetEnvironment {
            basePosition {
              type
              name
              coordinates {
                ra  { microseconds }
                dec { microarcseconds }
              }
            }
          }
        }
      }
    """

    def coords(raDeg: Double, decDeg: Double): Coordinates =
      Coordinates(
        RightAscension.fromDoubleDegrees(raDeg),
        Declination.fromDoubleDegrees(decDeg).get
      )

    val cA = coords(14.999, 29.999)
    val cB = coords(15.001, 30.000)
    val cC = coords(15.000, 30.001)
    val expected = Coordinates.centerOf(NonEmptyList.of(cA, cB, cC))

    for
      pid <- createProgramAs(pi)
      t1  <- createTargetAs(pi, pid, "A")
      t2  <- createTargetAs(pi, pid, "B")
      t3  <- createTargetAs(pi, pid, "C")
      _   <- setCoords(t1, "14.999", "29.999")
      _   <- setCoords(t2, "15.001", "30.000")
      _   <- setCoords(t3, "15.000", "30.001")
      oid <- createObservationAs(pi, pid, t1, t2, t3)
      _   <- setObservationTimeAndDuration(pi, oid, obsTime.some, duration.some)
      js  <- query(pi, basePosQuery(oid))
    yield
      val bp = js.hcursor.downFields("observation", "targetEnvironment", "basePosition")
      assertEquals(bp.downField("type").require[String], "ASTERISM")
      assertEquals(bp.downField("name").require[String], "A, B, C")
      val ra = bp.downFields("coordinates", "ra",  "microseconds").require[Long]
      val dec = bp.downFields("coordinates", "dec", "microarcseconds").require[Long]
      assertEquals(ra, expected.ra.toHourAngle.toMicroseconds)
      assertEquals(dec, expected.dec.toAngle.toMicroarcseconds)

  test("[baseposition] asterism with explicit base"):
    for
      pid <- createProgramAs(pi)
      t1  <- createTargetAs(pi, pid, "A")
      t2  <- createTargetAs(pi, pid, "B")
      oid <- createObservationAs(pi, pid, t1, t2)
      _   <- updateObservation(pi, oid, explicitBaseUpdate)
      _   <- expect(pi, query(oid), json"""
        {
          "observation": {
            "targetEnvironment": {
              "basePosition": {
                "type": "EXPLICIT_BASE",
                "name": "A, B",
                "sidereal": null,
                "nonsidereal": null,
                "coordinates": {
                  "ra":  { "hours": 1.000000 },
                  "dec": { "degrees": 2.0 }
                }
              }
            }
          }
        }
      """.asRight)
    yield ()

  test("[baseposition] asterism with no observation time"):
    for
      pid <- createProgramAs(pi)
      t1  <- createTargetAs(pi, pid, "A")
      t2  <- createTargetAs(pi, pid, "B")
      oid <- createObservationAs(pi, pid, t1, t2)
      _   <- expect(
               pi,
               query(oid),
               List(s"Observation time is required to compute the asterism base position in observation $oid.").asLeft
             )
    yield ()

  test("[baseposition] asterism containing an opportunity target"):
    for
      pid  <- createProgramAs(pi)
      tSid <- createTargetAs(pi, pid, "Alpha")
      tOpp <- createOpportunityTargetAs(pi, pid, "ToO")
      oid  <- createObservationAs(pi, pid, tSid, tOpp)
      _    <- setObservationTimeAndDuration(pi, oid, obsTime.some, duration.some)
      _    <- expect(pi, query(oid), json"""
        {
          "observation": {
            "targetEnvironment": {
              "basePosition": null
            }
          }
        }
      """.asRight)
    yield ()

  test("[baseposition] name truncation"):
    val targetA = "A" * 30
    val targetB = "B" * 30

    for
      pid  <- createProgramAs(pi)
      tA   <- createTargetAs(pi, pid, targetA)
      tB   <- createTargetAs(pi, pid, targetB)
      oid  <- createObservationAs(pi, pid, tA, tB)
      _    <- setObservationTimeAndDuration(pi, oid, obsTime.some, duration.some)
      name <- query(pi, query(oid)).map: js =>
                js.hcursor
                  .downFields("observation", "targetEnvironment", "basePosition", "name")
                  .require[String]
    yield
      assertEquals(name.length, 48)
      assert(name.endsWith("..."))
      val expectedPrefix = (targetA + ", " + targetB).take(45)
      assertEquals(name, expectedPrefix + "...")
