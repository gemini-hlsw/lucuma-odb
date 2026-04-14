// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType.*
import lucuma.core.model.StandardUser
import lucuma.core.model.User

class createObservation_GhostIfu extends OdbSuite:

  val pi: StandardUser = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers: List[User] =
    List(pi)

  test("create GHOST IFU"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query =
            s"""
              mutation {
                createObservation(input: {
                  programId: "$pid"
                  SET: {
                    targetEnvironment: {
                      asterism: [ "$tid" ]
                    }
                    scienceRequirements: ${scienceRequirementsObject(GhostIfu)}
                    observingMode: {
                      ghostIfu: {
                        resolutionMode: STANDARD
                        red: {
                          exposureTimeMode: {
                            timeAndCount: {
                              time: { seconds: 10.0 }
                              count: 2
                              at: { nanometers: 500 }
                            }
                          }
                          explicitBinning: ONE_BY_TWO
                        }
                        blue: {
                          explicitReadMode: FAST
                        }
                        explicitIfu1Agitator: ENABLED
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      instrument
                      mode
                      ghostIfu {
                        resolutionMode
                        red {
                          exposureTimeMode {
                            timeAndCount {
                              time { seconds }
                              count
                              at { nanometers }
                            }
                          }
                          binning
                          defaultBinning
                          explicitBinning
                          readMode
                          defaultReadMode
                          explicitReadMode
                        }
                        blue {
                          exposureTimeMode {
                            timeAndCount {
                              time { seconds }
                              count
                              at { nanometers }
                            }
                          }
                          binning
                          defaultBinning
                          explicitBinning
                          readMode
                          defaultReadMode
                          explicitReadMode
                        }
                        ifu1Agitator
                        defaultIfu1Agitator
                        explicitIfu1Agitator
                        ifu2Agitator
                        defaultIfu2Agitator
                        explicitIfu2Agitator
                      }
                    }
                  }
                }
              }
            """,
          expected =
            json"""
              {
                "createObservation": {
                  "observation": {
                    "observingMode": {
                      "instrument": "GHOST",
                      "mode": "GHOST_IFU",
                      "ghostIfu": {
                        "resolutionMode": "STANDARD",
                        "red": {
                          "exposureTimeMode": {
                            "timeAndCount": {
                              "time": { "seconds": 10.000000 },
                              "count": 2,
                              "at": { "nanometers": 500.000 }
                            }
                          },
                          "binning": "ONE_BY_TWO",
                          "defaultBinning": "ONE_BY_ONE",
                          "explicitBinning": "ONE_BY_TWO",
                          "readMode": "MEDIUM",
                          "defaultReadMode": "MEDIUM",
                          "explicitReadMode": null
                        },
                        "blue": {
                          "exposureTimeMode": {
                            "timeAndCount": {
                              "time": { "seconds": 1.000000 },
                              "count": 5,
                              "at": { "nanometers": 500.000 }
                            }
                          },
                          "binning": "ONE_BY_ONE",
                          "defaultBinning": "ONE_BY_ONE",
                          "explicitBinning": null,
                          "readMode": "FAST",
                          "defaultReadMode": "SLOW",
                          "explicitReadMode": "FAST"
                        },
                        "ifu1Agitator": "ENABLED",
                        "defaultIfu1Agitator": "DISABLED",
                        "explicitIfu1Agitator": "ENABLED",
                        "ifu2Agitator": "DISABLED",
                        "defaultIfu2Agitator": "DISABLED",
                        "explicitIfu2Agitator": null
                      }
                    }
                  }
                }
              }
            """.asRight
        )

  // This test case revealed a flaw in the ETM mapping that lead to Grackle
  // adding a join which filtered out the GMOS results.  We add a new GMOS
  // north observation and rely on the fact that a GHOST IFU observation is
  // already in the database from the previous test case.  Both should be
  // found by a query that filters no observations.
  test("query observations with mixed modes, including GHOST IFU exposureTimeModes"):
    for
      p <- createProgramAs(pi)
      t <- createTargetAs(pi, p)
      o <- createGmosNorthImagingObservationAs(pi, p, t)
      _ <- expect(
             user = pi,
             query = s"""
               query {
                 observations() {
                   matches {
                     observingMode {
                       mode
                       ghostIfu {
                         resolutionMode
                         red {
                           exposureTimeMode {
                             timeAndCount { count }
                           }
                           binning
                         }
                         blue {
                           exposureTimeMode {
                             timeAndCount { count }
                           }
                           binning
                         }
                       }
                     }
                   }
                 }
               }
             """,
             expected = json"""
               {
                 "observations": {
                   "matches": [
                     {
                       "observingMode": {
                         "mode": "GHOST_IFU",
                         "ghostIfu": {
                           "resolutionMode": "STANDARD",
                           "red": {
                             "exposureTimeMode": {
                               "timeAndCount": {
                                 "count": 2
                               }
                             },
                             "binning": "ONE_BY_TWO"
                           },
                           "blue": {
                             "exposureTimeMode": {
                               "timeAndCount": {
                                 "count": 5
                               }
                             },
                             "binning": "ONE_BY_ONE"
                           }
                         }
                       }
                     },
                     {
                       "observingMode": {
                         "mode": "GMOS_NORTH_IMAGING",
                         "ghostIfu": null
                       }
                     }
                   ]
                 }
               }
             """.asRight
           )
    yield ()

  test("cannot create GHOST IFU with S/N requirements only"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query =
            // N.B. Using the F2 science requirements to get a S/N ETM
            s"""
              mutation {
                createObservation(input: {
                  programId: "$pid"
                  SET: {
                    targetEnvironment: {
                      asterism: [ "$tid" ]
                    }
                    scienceRequirements: ${scienceRequirementsObject(Flamingos2LongSlit)}
                    observingMode: {
                      ghostIfu: {
                        resolutionMode: STANDARD
                      }
                    }
                  }
                }) {
                  observation { id }
                }
              }
            """,
          expected = List(
            "GHOST observations require a TimeAndCount exposure time mode."
          ).asLeft
        )

  test("can create GHOST IFU with S/N requirements if both detectors are specified as time and count"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(
          user  = pi,
          query =
            // N.B. Using the F2 science requirements to get a S/N ETM
            s"""
              mutation {
                createObservation(input: {
                  programId: "$pid"
                  SET: {
                    targetEnvironment: {
                      asterism: [ "$tid" ]
                    }
                    scienceRequirements: ${scienceRequirementsObject(Flamingos2LongSlit)}
                    observingMode: {
                      ghostIfu: {
                        resolutionMode: STANDARD
                        red: {
                          exposureTimeMode: {
                            timeAndCount: {
                              time: { seconds: 1.0 }
                              count: 1
                              at: { nanometers: 500 }
                            }
                          }
                        }
                        blue: {
                          exposureTimeMode: {
                            timeAndCount: {
                              time: { seconds: 2.0 }
                              count: 2
                              at: { nanometers: 500 }
                            }
                          }
                        }
                      }
                    }
                  }
                }) {
                  observation {
                    observingMode {
                      instrument
                      mode
                    }
                  }
                }
              }
            """,
          expected =
            json"""
              {
                "createObservation": {
                  "observation": {
                    "observingMode": {
                      "instrument": "GHOST",
                      "mode": "GHOST_IFU"
                    }
                  }
                }
              }
            """.asRight
        )