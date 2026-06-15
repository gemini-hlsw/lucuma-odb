// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GuideProbe
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan

class guideEnvironmentF2 extends ExecutionTestSupportForFlamingos2
                              with GuideEnvironmentSuite:

  override val fullTimeEstimate: TimeSpan = TimeSpan.fromMinutes(40).get

  private def flamingos2ImagingResult(
    title:    String,
    probe:    GuideProbe,
    posAngle: BigDecimal
  ): Either[List[String], Json] =
    json"""
      {
        "observation": {
          "title": $title,
          "targetEnvironment": {
            "guideEnvironment": {
              "posAngle": {
                "degrees": ${posAngle.setScale(6, BigDecimal.RoundingMode.HALF_UP)}
              },
              "guideTargets": [
                {
                  "name": "Gaia DR3 3219118090462918016",
                  "probe": ${probe.asJson},
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

  override def createObservationAs(user: User, pid: Program.Id, tids: List[Target.Id]): IO[Observation.Id] =
    createFlamingos2LongSlitObservationAs(user, pid, tids)

  test("no name set - let AGS pick best star"):
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      } yield o

    setup.flatMap: oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = json"""
          {
            "observation": {
              "title": "V1647 Orionis",
              "targetEnvironment": {
                "guideEnvironment": {
                  "posAngle": {
                    "degrees": 100.000000
                  },
                  "guideTargets": [
                    {
                      "name": "Gaia DR3 3219142829474535424",
                      "probe": "FLAMINGOS2_OIWFS",
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
                          "ra": {
                            "microarcsecondsPerYear": -10331,
                            "milliarcsecondsPerYear": -10.331
                          },
                          "dec": {
                            "microarcsecondsPerYear": -29666,
                            "milliarcsecondsPerYear": -29.666
                          }
                        },
                        "parallax": {
                          "microarcseconds": 2497,
                          "milliarcseconds": 2.497
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
      )

  test("nonsidereal target with PWFS2"):
    val setup: IO[Observation.Id] =
      for
        p   <- createProgramAs(pi)
        eph  = createNonsiderealEphemeris
        t   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, p, eph, name = "Nonsidereal Target")
        o   <- createObservationAs(pi, p, List(t))
        _   <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = pwfs2Result)

  test("Flamingos2 Imaging with oiwfs"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2ImagingObservationAs(pi, p, t)
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = flamingos2ImagingResult("V1647 Orionis", GuideProbe.Flamingos2OIWFS, 300.0))

  test("Flamingos2 Imaging with pwfs2"):
    val setup: IO[Observation.Id] =
      for
        p   <- createProgramAs(pi)
        eph  = createNonsiderealEphemeris
        t   <- createNonsiderealTargetWithUserSuppliedEphemerisAs(pi, p, eph, name = "Nonsidereal Target")
        o   <- createFlamingos2ImagingObservationAs(pi, p, t)
        _   <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      yield o

    setup.flatMap: oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = flamingos2ImagingResult("Nonsidereal Target", GuideProbe.PWFS2, 250.0))
