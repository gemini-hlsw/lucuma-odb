// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan

class guideEnvironmentGhost extends ExecutionTestSupportForGhost
                                    with GuideEnvironmentSuite:
  override val fullTimeEstimate: TimeSpan = TimeSpan.fromMinutes(40).get

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createGhostIfuObservationAs(user, pid, None, tids*)

  def setObservationPAC(
    user: User,
    oid:  Observation.Id,
  ): IO[Unit] =
    expect(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              WHERE: {
                id: {
                  EQ: ${oid.asJson}
                }
              }
              SET: {
                posAngleConstraint: {
                  mode: FIXED,
                  angle: {
                    degrees: 0.0
                  }
                }
              }
              includeDeleted: true
            }
          ) {
            observations {
              id
              posAngleConstraint {
                mode
                angle {
                  degrees
                }
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "updateObservations": {
            "observations": [
              { "id": $oid, "posAngleConstraint": { "mode": "FIXED", "angle": { "degrees": 0 } } }
            ]
          }
        }
      """.asRight
    )


  private def ghostPwfs2Result(title: String, degrees: BigDecimal): Either[List[String], Json] =
    json"""
    {
      "observation": {
        "title": $title,
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": $degrees
            },
            "guideTargets": [
              {
                "name": "Gaia DR3 3219118090462918016",
                "probe": "PWFS2",
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "brightnesses": [
                        {
                          "band": "GAIA"
                        },
                        {
                          "band": "GAIA_RP"
                        }
                      ]
                    }
                  }
                },
                "sidereal": {
                  "catalogInfo": {
                    "name": "GAIA",
                    "id": "3219118090462918016",
                    "objectType": null
                  },
                  "epoch": "J2016.000",
                  "ra": {
                    "microseconds": 20782433789,
                    "hms": "05:46:22.433789",
                    "hours": 5.772898274722222222222222222222222,
                    "degrees": 86.59347412083333333333333333333333
                  },
                  "dec": {
                    "dms": "-00:08:52.645460",
                    "degrees": 359.8520429277778,
                    "microarcseconds": 1295467354540
                  },
                  "radialVelocity": {
                    "metersPerSecond": 10090.042000,
                    "centimetersPerSecond": 1009004,
                    "kilometersPerSecond": 10.090042
                  },
                  "properMotion": {
                    "ra": {
                      "microarcsecondsPerYear": 438,
                      "milliarcsecondsPerYear": 0.438
                    },
                    "dec": {
                      "microarcsecondsPerYear": -741,
                      "milliarcsecondsPerYear": -0.741
                    }
                  },
                  "parallax": {
                    "microarcseconds": 2432,
                    "milliarcseconds": 2.432
                  }
                },
                "nonsidereal": null
              }
            ]
          }
        }
      }
    }
    """.asRight

  test("sidereal target - AGS picks best star with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setObservationPAC(pi, o)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = ghostPwfs2Result("V1647 Orionis", BigDecimal("0.000000")))

  test("nonsidereal target with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p   <- createProgramAs(pi)
        eph  = createNonsiderealEphemeris
        t   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, p, eph, name = "Nonsidereal Target")
        o   <- createObservationAs(pi, p, List(t))
        _   <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _   <- setObservationPAC(pi, o)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = ghostPwfs2Result("Nonsidereal Target", BigDecimal("0.000000")))

  // Creates a GHOST observation whose sky fiber position is set
  private def createGhostIfuObservationWithSkyPositionAs(
    user:          User,
    pid:           Program.Id,
    skyRaDegrees:  Double,
    skyDecDegrees: Double,
    tids:          Target.Id*
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids.toList,
      s"""
        ghostIfu: {
          stepCount: 1
          resolutionMode: STANDARD
          red: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 1 }
                count: 1
                at: { nanometers: 500 }
              }
            }
          }
          blue: {
            exposureTimeMode: {
              timeAndCount: {
                time: { seconds: 1 }
                count: 1
                at: { nanometers: 500 }
              }
            }
          }
          skyPosition: {
            ra: { degrees: $skyRaDegrees }
            dec: { degrees: $skyDecDegrees }
          }
        }
      """
    )

  // Result asserted by the "sky position is included as a science position for
  // AGS" test: with the sky fiber blocking the normally-preferred star, AGS picks
  // the next-best star 3219118640218737920 instead.
  private def ghostSkyPositionBlocksBestStarResult: Either[List[String], Json] =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": 0.000000
            },
            "guideTargets": [
              {
                "name": "Gaia DR3 3219118640218737920",
                "probe": "PWFS2",
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "brightnesses": [
                        {
                          "band": "GAIA"
                        },
                        {
                          "band": "GAIA_RP"
                        }
                      ]
                    }
                  }
                },
                "sidereal": {
                  "catalogInfo": {
                    "name": "GAIA",
                    "id": "3219118640218737920",
                    "objectType": null
                  },
                  "epoch": "J2016.000",
                  "ra": {
                    "microseconds": 20760247957,
                    "hms": "05:46:00.247957",
                    "hours": 5.766735543611111111111111111111111,
                    "degrees": 86.50103315416666666666666666666667
                  },
                  "dec": {
                    "dms": "-00:08:26.290793",
                    "degrees": 359.85936366861114,
                    "microarcseconds": 1295493709207
                  },
                  "radialVelocity": {
                    "metersPerSecond": 0,
                    "centimetersPerSecond": 0,
                    "kilometersPerSecond": 0
                  },
                  "properMotion": {
                    "ra": {
                      "microarcsecondsPerYear": 806,
                      "milliarcsecondsPerYear": 0.806
                    },
                    "dec": {
                      "microarcsecondsPerYear": -1093,
                      "milliarcsecondsPerYear": -1.093
                    }
                  },
                  "parallax": {
                    "microarcseconds": 2371,
                    "milliarcseconds": 2.371
                  }
                },
                "nonsidereal": null
              }
            ]
          }
        }
      }
    }
    """.asRight

  test("sky position is included as a science position for AGS"):
    // Place the GHOST sky fiber exactly on the guide star AGS would normally pick
    // (Gaia DR3 3219118090462918016). 
    // Treating the sky position as a science position makes that star unselectable thus
    // a different star (or none) must be returned. 
    // Without the fix, this still returns 3219118090462918016.
    val skyStarRa  = 86.5934741222927
    val skyStarDec = -0.14795707230703778

    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostIfuObservationWithSkyPositionAs(pi, p, skyStarRa, skyStarDec, t)
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setObservationPAC(pi, o)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = ghostSkyPositionBlocksBestStarResult)
