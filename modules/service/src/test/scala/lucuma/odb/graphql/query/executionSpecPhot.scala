// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionSpecPhot extends ExecutionTestSupportForGmos {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  test("spec phot") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.SpectroPhotometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
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
                             telescopeConfig {
                               offset {
                                 p { arcseconds }
                                 q { arcseconds }
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
           """,
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
                              "observeClass": "NIGHT_CAL",
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 1.000000
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
                                  "grating" : "R831_G5302",
                                  "order" : "ONE",
                                  "wavelength" : {
                                    "nanometers" : 500.000
                                  }
                                },
                                "filter": "R_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
                                }
                              },
                              "telescopeConfig": {
                                "offset": {
                                  "p": {
                                    "arcseconds": 0.000000
                                  },
                                  "q": {
                                    "arcseconds": 0.000000
                                  }
                                }
                              }
                            },
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 1200.000000
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
                              "telescopeConfig": {
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
  }

  test("spec phot, small custom wavelength dither") {
    // simultaneous coverage 235 nm, so dither up to 23.5 nm is ignored
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o <- createObservationWithModeAs(pi, p, List(t),
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitWavelengthDithers: [
                     {
                       nanometers:  0.0
                     },
                     {
                       nanometers: 10.0
                     },
                     {
                       nanometers: 23.5
                     }
                   ],
                   explicitYBin: TWO
                 }
               """
             )
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.SpectroPhotometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
                                 }
                               }
                             }
                           }
                         }
                         possibleFuture {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
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
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "steps": [
                            {
                              "observeClass": "NIGHT_CAL",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength" : {
                                    "nanometers" : 500.000
                                  }
                                }
                              }
                            },
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength": {
                                    "nanometers": 500.000
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
  }

  test("spec phot, large custom wavelength dither") {
    // simultaneous coverage 235 nm, so dither over 23.5 nm is tracked
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o <- createObservationWithModeAs(pi, p, List(t),
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitWavelengthDithers: [
                     {
                       nanometers:  0.0
                     },
                     {
                       nanometers: 23.501
                     }
                   ],
                   explicitYBin: TWO
                 }
               """
             )
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.SpectroPhotometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
                                 }
                               }
                             }
                           }
                         }
                         possibleFuture {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
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
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "steps": [
                            {
                              "observeClass": "NIGHT_CAL",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength" : {
                                    "nanometers" : 500.000
                                  }
                                }
                              }
                            },
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength": {
                                    "nanometers": 500.000
                                  }
                                }
                              }
                            }
                          ]
                        },
                        "possibleFuture": [
                          {
                            "steps": [
                              {
                                "observeClass": "NIGHT_CAL",
                                "instrumentConfig": {
                                  "gratingConfig": {
                                    "wavelength" : {
                                      "nanometers" : 523.501
                                    }
                                  }
                                }
                              },
                              {
                                "observeClass": "SCIENCE",
                                "instrumentConfig": {
                                  "gratingConfig": {
                                    "wavelength": {
                                      "nanometers": 523.501
                                    }
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
    }
  }

}
