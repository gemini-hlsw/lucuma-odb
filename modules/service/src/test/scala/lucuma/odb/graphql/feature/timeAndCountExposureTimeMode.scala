// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package feature

import cats.effect.IO
import cats.syntax.either.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class timeAndCountExposureTimeMode extends ExecutionTestSupportForGmos:

  def createGmosNorthLongSlitObservation(
    p: Program.Id,
    t: Target.Id
  ): IO[Observation.Id] =
    query(
      user  = pi,
      query =
      s"""
         mutation {
           createObservation(input: {
             programId: ${p.asJson},
             SET: {
               constraintSet: {
                 cloudExtinction: POINT_ONE,
                 imageQuality: ONE_POINT_ZERO,
                 skyBackground: DARKEST
               },
               targetEnvironment: {
                 asterism: ${List(t).asJson}
               },
               scienceRequirements: {
                 exposureTimeMode: {
                   timeAndCount: {
                     time: { minutes: 10 },
                     count: 3,
                     at: { nanometers: 500 }
                   }
                 },
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
                   fpu: LONG_SLIT_1_00,
                   centralWavelength: {
                     nanometers: 500
                   }
                 }
               }
             }
           }) {
             observation { id }
           }
         }
      """
    ).map: json =>
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]

  val setup: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservation(p, t)
    yield o

  test("specify exposure time and count"):
    setup.flatMap: o =>
      expect(
        user     = pi,
        query    = s"""
          query {
            observation(observationId: "$o") {
              scienceRequirements {
                 exposureTimeMode {
                   timeAndCount {
                     time { minutes }
                     count
                     at { nanometers }
                   }
                 }
              }
            }
          }
        """,
        expected = json"""
          {
            "observation": {
              "scienceRequirements": {
                "exposureTimeMode": {
                  "timeAndCount": {
                    "time": { "minutes":  10.000000 },
                    "count": 3,
                    "at": { "nanometers":  500.000 }
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("delete time and count"):
    setup.flatMap: o =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            updateObservations(input: {
              SET: {
                scienceRequirements: {
                  exposureTimeMode: null
                }
              },
              WHERE: {
                id: { EQ: "$o" }
              }
            }) {
              observations {
                scienceRequirements {
                  exposureTimeMode {
                    signalToNoise {
                      value
                    }
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
                {
                  "scienceRequirements": {
                    "exposureTimeMode": null
                  }
                }
              ]
            }
          }
        """.asRight
      )

  test("generate time and count sequence"):
    setup.flatMap: o =>
      expect(
        user     = pi,
        query    = s"""
          query {
            observation(observationId: "$o") {
              execution {
                config(futureLimit: 2) {
                  gmosNorth {
                    science {
                      nextAtom {
                        description
                        steps {
                          instrumentConfig {
                            exposure { seconds }
                          }
                          stepConfig { stepType }
                          telescopeConfig {
                            offset {
                              q { arcseconds }
                            }
                          }
                        }
                      }
                      possibleFuture {
                        description
                        steps {
                          instrumentConfig {
                            exposure { seconds }
                          }
                          stepConfig { stepType }
                          telescopeConfig {
                            offset {
                              q { arcseconds }
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
        """,
        expected = json"""
          {
            "observation": {
              "execution": {
                "config": {
                  "gmosNorth": {
                    "science": {
                      "nextAtom": {
                        "description": "0.000 nm",
                        "steps": [
                          {
                            "instrumentConfig": {
                              "exposure": { "seconds":  1.000000 }
                            },
                            "stepConfig": { "stepType": "GCAL" },
                            "telescopeConfig": {
                              "offset": {
                                "q": { "arcseconds": 0.000000 }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "exposure": { "seconds":  1.000000 }
                            },
                            "stepConfig": { "stepType": "GCAL" },
                            "telescopeConfig": {
                              "offset": {
                                "q": { "arcseconds": 0.000000 }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "exposure": { "seconds":  600.000000 }
                            },
                            "stepConfig": { "stepType": "SCIENCE" },
                            "telescopeConfig": {
                              "offset": {
                                "q": { "arcseconds": 0.000000 }
                              }
                            }
                          }
                        ]
                      },
                      "possibleFuture": [
                        {
                          "description": "5.000 nm",
                          "steps": [
                            {
                              "instrumentConfig": {
                                "exposure": { "seconds":  1.000000 }
                              },
                              "stepConfig": { "stepType": "GCAL" },
                              "telescopeConfig": {
                                "offset": {
                                  "q": { "arcseconds": 15.000000 }
                                }
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": { "seconds":  1.000000 }
                              },
                              "stepConfig": { "stepType": "GCAL" },
                              "telescopeConfig": {
                                "offset": {
                                  "q": { "arcseconds": 15.000000 }
                                }
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": { "seconds":  600.000000 }
                              },
                              "stepConfig": { "stepType": "SCIENCE" },
                              "telescopeConfig": {
                                "offset": {
                                  "q": { "arcseconds": 15.000000 }
                                }
                              }
                            }
                          ]
                        },
                        {
                          "description": "-5.000 nm",
                          "steps": [
                            {
                              "instrumentConfig": {
                                "exposure": { "seconds":  1.000000 }
                              },
                              "stepConfig": { "stepType": "GCAL" },
                              "telescopeConfig": {
                                "offset": {
                                  "q": { "arcseconds": -15.000000 }
                                }
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": { "seconds":  1.000000 }
                              },
                              "stepConfig": { "stepType": "GCAL" },
                              "telescopeConfig": {
                                "offset": {
                                  "q": { "arcseconds": -15.000000 }
                                }
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": { "seconds":  600.000000 }
                              },
                              "stepConfig": { "stepType": "SCIENCE" },
                              "telescopeConfig": {
                                "offset": {
                                  "q": { "arcseconds": -15.000000 }
                                }
                              }
                            }
                          ]
                        }
                      ]
                    }
                  }
                }
              }
            }
          }
        """.asRight
      )

  test("cannot generate time and count sequence with 0 second exposure time"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      _ <- expect(
        user  = pi,
        query = s"""
        mutation {
         createObservation(input: {
             programId: ${p.asJson},
             SET: {
               constraintSet: {
                 cloudExtinction: POINT_ONE,
                 imageQuality: ONE_POINT_ZERO,
                 skyBackground: DARKEST
               },
               targetEnvironment: {
                 asterism: ${List(t).asJson}
               },
               scienceRequirements: {
                 exposureTimeMode: {
                   timeAndCount: {
                     time: { minutes: 0 },
                     count: 3,
                     at: { nanometers: 500 }
                   }
                 },
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
                   fpu: LONG_SLIT_1_00,
                   centralWavelength: {
                     nanometers: 500
                   }
                 }
               }
             }
           }) {
             observation { id }
           }
         }
        """,
        expected = List(
          "Argument 'input.SET.scienceRequirements.exposureTimeMode.timeAndCount' is invalid: Exposure `time` parameter must be positive."
        ).asLeft
      )
    yield ()
