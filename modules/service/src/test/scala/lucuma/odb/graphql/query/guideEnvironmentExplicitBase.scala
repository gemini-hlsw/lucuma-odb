// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

// Tests that an explicit base, when set, is used as the centre of the Gaia
// guide-star query and for AGS analysis
class guideEnvironmentExplicitBase extends ExecutionTestSupportForGmos
                                     with GuideEnvironmentSuite:

  override def createObservationAs(
    user: User,
    pid:  Program.Id,
    tids: List[Target.Id]
  ): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids, offsetArcsec = List(0, 15).some)

  private def setExplicitBase(
    user:  User,
    oid:   Observation.Id,
    raDeg: Double,
    decDeg: Double
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(input: {
            SET: {
              targetEnvironment: {
                explicitBase: {
                  ra:  { degrees: "$raDeg" },
                  dec: { degrees: "$decDeg" }
                }
              }
            }
            WHERE: { id: { EQ: ${oid.asJson} } }
          }) {
            observations { id }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [ { "id": $oid } ]
          }
        }
      """.asRight
    )

  // Result when the explicit base is shifted ~100 arcsec north:
  // AGS prefers star 3219142829474535424 instead of the usual 3219118090462918016
  private val shiftedBaseResult =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": { "degrees": 100.000000 },
            "guideTargets": [{
              "name": "Gaia DR3 3219142829474535424",
              "probe": "GMOS_OIWFS",
              "sourceProfile": {
                "point": {
                  "bandNormalized": {
                    "brightnesses": [
                      { "band": "GAIA" },
                      { "band": "GAIA_RP" }
                    ]
                  }
                }
              },
              "sidereal": {
                "catalogInfo": {
                  "name": "GAIA",
                  "id": "3219142829474535424",
                  "objectType": null
                },
                "epoch": "J2016.000",
                "ra": {
                  "microseconds": 20782389077,
                  "hms": "05:46:22.389077",
                  "hours": 5.772885854722222222222222222222222,
                  "degrees": 86.59328782083333333333333333333333
                },
                "dec": {
                  "dms": "-00:03:38.722655",
                  "degrees": 359.93924370694447,
                  "microarcseconds": 1295781277345
                },
                "radialVelocity": {
                  "metersPerSecond": 0,
                  "centimetersPerSecond": 0,
                  "kilometersPerSecond": 0
                },
                "properMotion": {
                  "ra": { "microarcsecondsPerYear": -10331, "milliarcsecondsPerYear": -10.331 },
                  "dec": { "microarcsecondsPerYear": -29666, "milliarcsecondsPerYear": -29.666 }
                },
                "parallax": { "microarcseconds": 2497, "milliarcseconds": 2.497 }
              },
              "nonsidereal": null
            }]
          }
        }
      }
    }
    """.asRight

  test("without explicit base - AGS picks guide star 3219118090462918016"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = successfulGuideEnvironmentResult)

  test("with explicit base shifted 100 arcsec north - AGS picks a different guide star"):
    // Shifting the explicit base 100 arcsec north moves the probe patrol field
    // enough that AGS prefers a different guide star (3219142829474535424 at PA
    // 100° instead of 3219118090462918016 at PA 180°).
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        // base RA unchanged; dec shifted +100 arcsec north
        _ <- setExplicitBase(pi, o, 86.55474, -0.07359)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = shiftedBaseResult)
