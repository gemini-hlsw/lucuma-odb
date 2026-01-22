// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.Stream
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import org.http4s.Request
import org.http4s.Response

class guideEnvironmentF2 extends ExecutionTestSupportForFlamingos2
                              with GuideEnvironmentSuite:

  override val fullTimeEstimate: TimeSpan = TimeSpan.fromMinutes(40).get

  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    _ => Resource.eval(IO.pure(Response(body = Stream(gaiaResponseString).through(utf8.encode))))

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

  val pwfs2Result =
    json"""
    {
      "observation": {
        "title": "Nonsidereal Target",
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": 350.000000
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

  test("nonsidereal target uses PWFS2 for guiding"):
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
