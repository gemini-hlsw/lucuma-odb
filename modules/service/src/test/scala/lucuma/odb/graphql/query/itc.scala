// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.json.wavelength.transport.given

class itc extends OdbSuite with ObservingModeSetupOperations {

  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "ITC Testing")

  def setup(targetCount: Int = 1): IO[(Program.Id, Observation.Id, List[Target.Id])] =
    for {
      p  <- createProgram
      ts <- (1 to targetCount).toList.traverse(_ => createTargetWithProfileAs(user, p))
      o  <- createGmosNorthLongSlitObservationAs(user, p, ts)
    } yield (p, o, ts)

  def setup1: IO[(Program.Id, Observation.Id, Target.Id)] =
    setup(1).map {
      case (pid, oid, List(tid)) => (pid, oid, tid)
      case _                     => sys.error("Expected a single target")
    }

  def setup2: IO[(Program.Id, Observation.Id, Target.Id, Target.Id)] =
    setup(2).map {
      case (pid, oid, List(tid0, tid1)) => (pid, oid, tid0, tid1)
      case _                            => sys.error("Expected two targets")
    }

  def itcQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          id
          itc {
            ... on ItcSpectroscopy {
              spectroscopyScience {
                selected {
                  targetId
                  exposureTime {
                    seconds
                  }
                  exposureCount
                  signalToNoiseAt {
                    wavelength {
                      picometers
                    }
                    single
                    total
                  }
                }
                all {
                  targetId
                }
              }
              acquisition {
                selected {
                  targetId
                  exposureTime {
                    seconds
                  }
                  exposureCount
                  signalToNoiseAt {
                    wavelength {
                      picometers
                    }
                    single
                    total
                  }
                }
                all {
                  targetId
                }
              }
            }
          }
        }
      }
    """


  def successfulItcResult(oid: Observation.Id, tid: Target.Id): Json =
    json"""
      {
        "observation": {
          "id": $oid,
          "itc": {
            "spectroscopyScience": {
              "selected": {
              "targetId": $tid,
                "exposureTime": {
                  "seconds": 10.000000
                },
                "exposureCount": ${FakeItcResult.exposureCount.value},
                "signalToNoiseAt": ${fakeSignalToNoiseAt(Wavelength.fromIntNanometers(500).get).asJson}
              },
              "all": [
                {
                  "targetId": $tid
                }
              ]
            },
            "acquisition": {
              "selected": {
                "targetId": $tid,
                "exposureTime": {
                  "seconds": 10.000000
                },
                "exposureCount": ${FakeItcResult.exposureCount.value},
                "signalToNoiseAt": null
              },
              "all": [
                {
                  "targetId": $tid
                }
              ]
            }
          }
        }
      }
    """

  test("success, one target") {
    setup1.flatMap { case (_, oid, tid) =>
      expect(
        user = user,
        query = itcQuery(oid),
        expected = successfulItcResult(oid, tid).asRight
      )
    }
  }

  test("success, two targets") {
    setup2.flatMap { case (_, oid, tid0, tid1) =>
      expect(
        user = user,
        query =
          s"""
            query {
              observation(observationId: "$oid") {
                itc {
                  ... on ItcSpectroscopy {
                    spectroscopyScience {
                      selected {
                        targetId
                      }
                      all {
                        targetId
                      }
                    }
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "observation": {
                "itc": {
                  "spectroscopyScience": {
                    "selected": {
                      "targetId": $tid0
                    },
                    "all": [
                      {
                        "targetId": $tid0
                      },
                      {
                        "targetId": $tid1
                      }
                    ]
                  }
                }
              }
            }
          """
        )
      )
    }
  }

  test("success, point emission lines target") {
    for {
      pid <- createProgram
      tid <- createTargetWithProfileAs(user, pid, PointEmissionLinesProfile)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      r   <- expect(
        user = user,
        query = itcQuery(oid),
        expected = successfulItcResult(oid, tid).asRight
      )
    } yield r
  }

  test("success, uniform emission lines target") {
    for {
      pid <- createProgram
      tid <- createTargetWithProfileAs(user, pid, UniformEmissionLinesProfile)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      r   <- expect(
        user = user,
        query = itcQuery(oid),
        expected = successfulItcResult(oid, tid).asRight
    )
  } yield r
}

  test("observation missing observingMode") {
    def createObservation(pid: Program.Id, tid: Target.Id): IO[Observation.Id] =
      query(
        user  = user,
        query =
        s"""
           mutation {
             createObservation(input: {
               programId: ${pid.asJson},
               SET: {
                 constraintSet: {
                   cloudExtinction: POINT_ONE,
                   imageQuality: POINT_ONE,
                   skyBackground: DARKEST
                 },
                 targetEnvironment: {
                   asterism: [ ${tid.asJson} ]
                 },
                 scienceRequirements: {
                   exposureTimeMode: {
                      signalToNoise: {
                        value: 100.0
                        at: { nanometers: 500 }
                      }
                   }
                   spectroscopy: {
                     wavelength: {
                       nanometers: 500
                     },
                     resolution: 100,
                     wavelengthCoverage: {
                       nanometers: 20
                     },
                     focalPlane: SINGLE_SLIT,
                     focalPlaneAngle: {
                       microarcseconds: 0
                     }
                   }
                 }
               }
             }) {
               observation {
                 id
               }
             }
           }
        """
      ).map { json =>
        json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
      }

    for {
      p <- createProgram
      t <- createTargetWithProfileAs(user, p)
      o <- createObservation(p, t)
      r <- expect(
        user = user,
        query =
          s"""
            query {
              observation(observationId: "$o") {
                itc { itcType }
              }
            }
          """,
        expected = Left(List(
            "ITC cannot be queried until the following parameters are defined: observing mode"
        ))
      )
    } yield r
  }

  test("observation missing targets") {
    def createObservation(pid: Program.Id): IO[Observation.Id] =
      query(
        user  = user,
        query =
        s"""
           mutation {
             createObservation(input: {
               programId: ${pid.asJson},
               SET: {
                 constraintSet: {
                   cloudExtinction: POINT_ONE,
                   imageQuality: POINT_ONE,
                   skyBackground: DARKEST
                 },
                 scienceRequirements: {
                   exposureTimeMode: {
                      signalToNoise: {
                        value: 100.0,
                        at: { nanometers: 500 }
                      }
                   }
                   spectroscopy: {
                     wavelength: {
                       nanometers: 500
                     },
                     resolution: 100,
                     wavelengthCoverage: {
                       nanometers: 20
                     },
                     focalPlane: SINGLE_SLIT,
                     focalPlaneAngle: {
                       microarcseconds: 0
                     }
                   }
                 },
                 observingMode: {
                   gmosNorthLongSlit: {
                     grating: R831_G5302,
                     filter: R_PRIME,
                     fpu: LONG_SLIT_0_50,
                     centralWavelength: {
                       nanometers: 500
                     }
                   }
                 }
               }
             }) {
               observation {
                 id
               }
             }
           }
        """
      ).map { json =>
        json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
      }

    for {
      p <- createProgram
      o <- createObservation(p)
      r <- expect(
        user = user,
        query =
          s"""
            query {
              observation(observationId: "$o") {
                itc { itcType }
              }
            }
          """,
        expected = Left(List(
          "ITC cannot be queried until the following parameters are defined: target"
        ))
      )
    } yield r
  }

  test("target missing rv and brightness") {

    def createTarget(pid: Program.Id): IO[Target.Id] =
      query(
        user  = user,
        query =
        s"""
          mutation {
            createTarget(input: {
              programId: ${pid.asJson},
              SET: {
                name: "V1647 Orionis"
                sidereal: {
                  ra: { hms: "05:46:13.137" },
                  dec: { dms: "-00:06:04.89" },
                  epoch: "J2000.0",
                  properMotion: {
                    ra: {
                      milliarcsecondsPerYear: 0.918
                    },
                    dec: {
                      milliarcsecondsPerYear: -1.057
                    },
                  },
                  parallax: {
                    milliarcseconds: 2.422
                  }
                },
                sourceProfile: {
                  point: {
                    bandNormalized: {
                      sed: {
                        stellarLibrary: O5_V
                      },
                      brightnesses: [
                      ]
                    }
                  }
                }
              }
            }) {
              target {
                id
              }
            }
          }
        """
      ).map(
        _.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
      )

    for {
      p <- createProgram
      t <- createTarget(p)
      o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      r <- expect(
        user = user,
        query =
          s"""
            query {
              observation(observationId: "$o") {
                itc { itcType }
              }
            }
          """,
        expected = Left(List(
          s"ITC cannot be queried until the following parameters are defined: brightness measure"
        ))
      )
    } yield r
  }

  test("point emission lines missing lines") {
    for {
      pid <- createProgram
      tid <- createTargetWithProfileAs(user, pid, PointEmissionLinesProfileNoLines)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      r   <- expect(
        user = user,
        query = itcQuery(oid),
        expected = Left(List(
          s"ITC cannot be queried until the following parameters are defined: brightness measure"
        ))
      )
    } yield r
  }

  test("uniform emission lines missing lines") {
    for {
      pid <- createProgram
      tid <- createTargetWithProfileAs(user, pid, UniformEmissionLinesProfileNoLines)
      oid <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      r   <- expect(
        user = user,
        query = itcQuery(oid),
        expected = Left(List(
          s"ITC cannot be queried until the following parameters are defined: brightness measure"
        ))
      )
    } yield r
  }

}
