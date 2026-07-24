// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.User

class createObservation_GnirsImaging extends OdbSuite:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("create GNIRS imaging"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 100.0
                      at: { nanometers: 1250.0 }
                    }
                  }
                }
                observingMode: {
                  gnirsImaging: {
                    camera: SHORT_BLUE
                    filters: [
                      { filter: J },
                      { filter: ORDER4 }
                    ]
                    explicitReadMode: BRIGHT
                  }
                }
              }
            }) {
              observation {
                instrument
                observingMode {
                  mode
                  gnirsImaging {
                    filters { filter }
                    initialFilters { filter }
                    camera
                    coadds
                    explicitReadMode
                    wellDepth
                    defaultWellDepth
                    explicitWellDepth
                  }
                }
              }
            }
          }
        """,
        json"""
          {
            "createObservation": {
              "observation": {
                "instrument": "GNIRS",
                "observingMode": {
                  "mode": "GNIRS_IMAGING",
                  "gnirsImaging": {
                    "filters": [
                      { "filter": "ORDER4" },
                      { "filter": "J" }
                    ],
                    "initialFilters": [
                      { "filter": "ORDER4" },
                      { "filter": "J" }
                    ],
                    "camera": "SHORT_BLUE",
                    "coadds": 1,
                    "explicitReadMode": "BRIGHT",
                    "wellDepth": "SHALLOW",
                    "defaultWellDepth": "SHALLOW",
                    "explicitWellDepth": null
                  }
                }
              }
            }
          }
        """.asRight)

  test("create GNIRS imaging without a camera fails"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                observingMode: {
                  gnirsImaging: {
                    filters: [
                      { filter: J }
                    ]
                  }
                }
              }
            }) {
              observation {
                id
              }
            }
          }
        """,
        List("Argument 'input.SET.observingMode.gnirsImaging' is invalid: A 'camera' is required on creation.").asLeft)

  test("create GNIRS imaging without a variant uses the defaults"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi, s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
              SET: {
                targetEnvironment: {
                  asterism: [${tid.asJson}]
                }
                scienceRequirements: {
                  exposureTimeMode: {
                    signalToNoise: {
                      value: 100.0
                      at: { nanometers: 1250.0 }
                    }
                  }
                }
                observingMode: {
                  gnirsImaging: {
                    camera: LONG_RED
                    filters: [
                      { filter: J }
                    ]
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gnirsImaging {
                    wellDepth
                    variant {
                      variantType
                      grouped {
                        order
                        skyCount
                        offsets {
                          generatorType
                          uniform {
                            cornerA { p { arcseconds } q { arcseconds } }
                            cornerB { p { arcseconds } q { arcseconds } }
                          }
                        }
                        skyOffsets {
                          generatorType
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """,
        json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gnirsImaging": {
                    "wellDepth": "DEEP",
                    "variant": {
                      "variantType": "GROUPED",
                      "grouped": {
                        "order": "INCREASING",
                        "skyCount": 0,
                        "offsets": {
                          "generatorType": "UNIFORM",
                          "uniform": {
                            "cornerA": { "p": { "arcseconds": 4 }, "q": { "arcseconds": 6 } },
                            "cornerB": { "p": { "arcseconds": -1 }, "q": { "arcseconds": -6 } }
                          }
                        },
                        "skyOffsets": {
                          "generatorType": "NONE"
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight)
