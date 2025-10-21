// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import fs2.Stream
import fs2.text.utf8
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.ags.GuideStarName
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import org.http4s.Request
import org.http4s.Response

class guideEnvironmentGMOS extends ExecutionTestSupportForGmos with GuideEnvironmentSuite {

  val gaiaEmpty: Timestamp = Timestamp.FromString.getOption("3000-01-30T04:00:00Z").get
  val gaiaError: Timestamp = Timestamp.FromString.getOption("4000-12-30T20:00:00Z").get

  val setupTime: TimeSpan = TimeSpan.fromMinutes(16).get
  val fullTimeEstimate: TimeSpan = TimeSpan.parse("PT36M1.8S").toOption.get
  val durationTooShort: TimeSpan = setupTime -| TimeSpan.fromMicroseconds(1).get
  val durationTooLong: TimeSpan = fullTimeEstimate +| TimeSpan.fromMicroseconds(1).get
  val durationNotValidated: TimeSpan = TimeSpan.Zero

  val invalidTargetId: Long = 1L
  val invalidTargetName: String =
    GuideStarName.gaiaSourceId.reverseGet(invalidTargetId).value.value

  val emptyGuideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironments": null
        }
      }
    }
    """.asRight

  val defaultGuideEnvironmentResults =
    json"""
    {
      "observation": {
        "title": "V1647 Orionis",
        "targetEnvironment": {
          "guideEnvironment": {
            "posAngle": {
              "degrees": 180.000000
            },
            "guideTargets": [
              {
                "name": "Gaia DR3 3219118090462918016",
                "probe": "GMOS_OIWFS",
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "brightnesses": [
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
                  "epoch": "J2023.660",
                  "ra": {
                    "microseconds": 20782434012,
                    "hms": "05:46:22.434012",
                    "hours": 5.772898336666666666666666666666667,
                    "degrees": 86.59347505
                  },
                  "dec": {
                    "dms": "-00:08:52.651136",
                    "degrees": 359.8520413511111,
                    "microarcseconds": 1295467348864
                  },
                  "radialVelocity": {
                    "metersPerSecond": 0,
                    "centimetersPerSecond": 0,
                    "kilometersPerSecond": 0
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

  val otherGuideEnvironmentResults =
    json"""
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
                "probe": "GMOS_OIWFS",
                "sourceProfile": {
                  "point": {
                    "bandNormalized": {
                      "brightnesses": [
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
                  "epoch": "J2023.660",
                  "ra": {
                    "microseconds": 20782383800,
                    "hms": "05:46:22.383800",
                    "hours": 5.772884388888888888888888888888889,
                    "degrees": 86.59326583333333333333333333333333
                  },
                  "dec": {
                    "dms": "-00:03:38.949911",
                    "degrees": 359.93918058027776,
                    "microarcseconds": 1295781050089
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

  override def httpRequestHandler: Request[IO] => Resource[IO, Response[IO]] =
    req => {
      val renderStr = req.uri.renderString
      if (renderStr.contains("20-0.10195")) {
        Resource.eval(IO.raiseError(Exception("Test failure, unexpected call to Gaia!!!")))
      } else {
        val respStr =
          if (renderStr.contains(defaultTargetId.toString)) gaiaDefaultNameResponseString
          else if (renderStr.contains(otherTargetId.toString)) gaiaOtherNameReponseString
          else if (renderStr.contains("20-0.10137")) gaiaResponseString
          else gaiaEmptyReponseString
        Resource.eval(IO.pure(Response(body = Stream(respStr).through(utf8.encode))))
      }
    }

  override def createObservationAs(
    user: User,
    pid: Program.Id,
    tids: List[Target.Id]
  ): IO[Observation.Id] =
    createGmosNorthLongSlitObservationAs(user, pid, tids, offsetArcsec = List(0, 15).some)

  test("no science targets") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        o <- createObservationAs(pi, p, List.empty)
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationNotValidated.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"No targets have been defined for observation $oid.").asLeft)
    }
  }

  test("no observation time") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Observation time not set for observation $oid.").asLeft)
    }
  }

  test("no observation duration") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, none)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Observation duration not set for observation $oid.").asLeft)
    }
  }

  test("observation duration too short") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationTooShort.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Observation duration of ${durationTooShort.format} is less than the setup time of ${setupTime.format} for observation $oid.").asLeft)
    }
  }

  // Temporarily(?) disable check for too long of a duration for sc-5322
  // test("observation duration too long") {
  //   val setup: IO[Observation.Id] =
  //     for {
  //       p <- createProgramAs(pi)
  //       t <- createTargetWithProfileAs(pi, p)
  //       o <- createObservationAs(pi, p, List(t))
  //       _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationTooLong.some)
  //     } yield o
  //   setup.flatMap { oid =>
  //     expect(
  //       pi,
  //       guideEnvironmentQuery(oid),
  //       expected = List(s"Observation duration of ${durationTooLong.format} exceeds the remaining time of ${fullTimeEstimate.format} for observation $oid.").asLeft)
  //   }
  // }

  test("no guide stars") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaEmpty.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List("No potential guidestars found on Gaia.").asLeft)
    }
  }

  test("no configuration") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
        _ <- setObservationTimeAndDuration(pi, o, gaiaError.some, durationNotValidated.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Could not generate a sequence for $oid: observation is missing observing mode").asLeft)
    }
  }

  test("non-existent name set") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setGuideTargetName(pi, o, invalidTargetName.some)
      } yield o
    setup.flatMap { oid =>
      expect(
        pi,
        guideEnvironmentQuery(oid),
        expected = List(s"Error calling Gaia: Star with id $invalidTargetId not found on Gaia.").asLeft)
    }
  }

  test("no name set - successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  test("no name set - successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  test("name set to default - call gaia") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setGuideTargetName(pi, o, defaultTargetName.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  test("name set to other usable - call gaia") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
        _ <- setGuideTargetName(pi, o, otherTargetName.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = otherGuideEnvironmentResults)
    }
  }

  test("with blind offset target - successfully obtain guide environment") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgramAs(pi)
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, List(t))
        _ <- setBlindOffsetViaGraphQL(pi, o, "Blind Offset Target")
        _ <- setObservationTimeAndDuration(pi, o, gaiaSuccess.some, fullTimeEstimate.some)
      } yield o
    setup.flatMap { oid =>
      expect(pi, guideEnvironmentQuery(oid), expected = defaultGuideEnvironmentResults)
    }
  }

  def setBlindOffsetViaGraphQL(user: User, oid: Observation.Id, blindOffsetName: String): IO[Unit] =
    query(
      user = user,
      query =
        s"""
          mutation {
            updateObservations(input: {
              WHERE: { id: { EQ: "$oid" } }
              SET: {
                targetEnvironment: {
                  blindOffsetTarget: {
                    name: ${blindOffsetName.asJson}
                    sidereal: {
                      ra: { degrees: "12.345" }
                      dec: { degrees: "45.678" }
                      epoch: "J2000.000"
                    }
                    sourceProfile: {
                      point: {
                        bandNormalized: {
                          sed: { stellarLibrary: B5_III }
                          brightnesses: [
                            {
                              band: R
                              value: 15.0
                              units: VEGA_MAGNITUDE
                            }
                          ]
                        }
                      }
                    }
                  }
                }
              }
            }) {
              observations { id }
            }
          }
        """
    ).void

}
