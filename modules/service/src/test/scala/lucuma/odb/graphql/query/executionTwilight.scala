// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionTwilight extends ExecutionTestSupport {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  val setup: IO[Observation.Id] =
    for
      p  <- createProgram
      t  <- twilightTargets.map(_.head)
      tʹ <- withServices(staff) { services =>
        services.session.transaction.use: xa =>
          services.targetService.cloneTargetInto(t, p)(using xa)
            .flatMap(_.toOption.liftTo[IO](new RuntimeException("cloneInto failure")))
            .map(_._2)
      }
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(tʹ))
      _  <- withServices(serviceUser) { services =>
             services.session.transaction.use: xa =>
               services.calibrationsService.setCalibrationRole(o, CalibrationRole.Twilight.some)(using xa)
           }
    yield o

  def query(sequenceType: String, oid: Observation.Id): String =
    s"""
       query {
         observation(observationId: "$oid") {
           execution {
             config {
               gmosNorth {
                 $sequenceType {
                   nextAtom {
                     observeClass
                     steps {
                       observeClass
                       instrumentConfig {
                         exposure {
                           seconds
                         }
                         readout {
                           xBin
                           yBin
                           ampCount
                           ampGain
                           ampReadMode
                         }
                         dtax
                         roi
                         gratingConfig {
                           grating
                           order
                           wavelength {
                             nanometers
                           }
                         }
                         filter
                         fpu {
                           builtin
                           customMask { slitWidth }
                         }
                       }
                       stepConfig {
                         ... on Science {
                           offset {
                             p { arcseconds }
                             q { arcseconds }
                           }
                         }
                       }
                     }
                   }
                   possibleFuture {
                     steps {
                       instrumentConfig {
                         exposure {
                           seconds
                         }
                       }
                     }
                   }
                 }
               }
             }
           }
         }
       }
     """

  test("twilight - science") {
    setup.flatMap: oid =>
      expect(
        user  = pi,
        query = query("science", oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "observeClass": "SCIENCE",
                          "steps": [
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 30.000000
                                },
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "TWO",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "SLOW"
                                },
                                "dtax": "ZERO",
                                "roi": "FULL_FRAME",
                                "gratingConfig": {
                                  "grating": "R831_G5302",
                                  "order": "ONE",
                                  "wavelength": {
                                    "nanometers": 500.000
                                  }
                                },
                                "filter": "R_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
                                }
                              },
                              "stepConfig": {
                                "offset": {
                                  "p": {
                                    "arcseconds": 0.000000
                                  },
                                  "q": {
                                    "arcseconds": 0.000000
                                  }
                                }
                              }
                            }
                          ]
                        },
                        "possibleFuture": []
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
  }

  test("twilight - acquisition") {
    setup.flatMap: oid =>
      expect(
        user  = pi,
        query = query("acquisition", oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "acquisition": null
                    }
                  }
                }
              }
            }
          """.asRight
      )
  }

}
