// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType.GhostIfu
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
                            signalToNoise {
                              value
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
                            "signalToNoise": {
                              "value": 100.000,
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