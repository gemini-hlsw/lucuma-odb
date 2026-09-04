// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
                    filters { filter coadds }
                    initialFilters { filter coadds }
                    camera
                    explicitReadMode
                    wellDepth
                    defaultWellDepth
                    explicitWellDepth
                    acquisition {
                      explicitAcquisitionType
                      coadds
                      explicitFilter
                      skyOffset { p { arcseconds } q { arcseconds } }
                      exposureTimeMode {
                        signalToNoise { value at { nanometers } }
                        timeAndCount { time { seconds } count at { nanometers } }
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
                "instrument": "GNIRS",
                "observingMode": {
                  "mode": "GNIRS_IMAGING",
                  "gnirsImaging": {
                    "filters": [
                      { "filter": "ORDER4", "coadds": 1 },
                      { "filter": "J", "coadds": 1 }
                    ],
                    "initialFilters": [
                      { "filter": "ORDER4", "coadds": 1 },
                      { "filter": "J", "coadds": 1 }
                    ],
                    "camera": "SHORT_BLUE",
                    "explicitReadMode": "BRIGHT",
                    "wellDepth": "SHALLOW",
                    "defaultWellDepth": "SHALLOW",
                    "explicitWellDepth": null,
                    "acquisition": {
                      "explicitAcquisitionType": null,
                      "coadds": 1,
                      "explicitFilter": null,
                      "skyOffset": null,
                      "exposureTimeMode": {
                        "signalToNoise": {
                          "value": 10.000,
                          "at": { "nanometers": 1250.000 }
                        },
                        "timeAndCount": null
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight)

  test("create GNIRS imaging with per-filter coadds"):
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).flatMap: tid =>
        expect(pi,
          s"""
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
                        {
                          filter: J
                          exposureTimeMode: {
                            timeAndCount: { time: { seconds: 30.0 }, count: 6, at: { nanometers: 1250.0 } }
                          }
                          coadds: 4
                        },
                        {
                          filter: ORDER4
                          exposureTimeMode: {
                            signalToNoise: { value: 50.0, at: { nanometers: 1250.0 } }
                          }
                          coadds: 7
                        }
                      ]
                    }
                  }
                }
              }) {
                observation {
                  observingMode {
                    gnirsImaging {
                      filters {
                        filter
                        coadds
                        exposureTimeMode {
                          signalToNoise { value at { nanometers } }
                          timeAndCount { time { seconds } count at { nanometers } }
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
                      "filters": [
                        {
                          "filter": "ORDER4",
                          "coadds": 1,
                          "exposureTimeMode": {
                            "signalToNoise": {
                              "value": 50.000,
                              "at": { "nanometers": 1250.000 }
                            },
                            "timeAndCount": null
                          }
                        },
                        {
                          "filter": "J",
                          "coadds": 4,
                          "exposureTimeMode": {
                            "signalToNoise": null,
                            "timeAndCount": {
                              "time": { "seconds": 30.000000 },
                              "count": 6,
                              "at": { "nanometers": 1250.000 }
                            }
                          }
                        }
                      ]
                    }
                  }
                }
              }
            }
          """.asRight)

  /** Creates a GNIRS imaging observation with the given acquisition input block. */
  private def createWithAcquisition(acquisition: String, selection: String) =
    createProgramAs(pi).flatMap: pid =>
      createTargetAs(pi, pid).map: tid =>
        s"""
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
                    filters: [ { filter: J } ]
                    acquisition: $acquisition
                  }
                }
              }
            }) {
              observation {
                observingMode {
                  gnirsImaging {
                    acquisition { $selection }
                  }
                }
              }
            }
          }
        """

  test("create GNIRS imaging with acquisition skyOffset — round-trips"):
    createWithAcquisition(
      """{
        explicitAcquisitionType: FAINT
        skyOffset: { p: { arcseconds: 1.5 }, q: { arcseconds: -2.5 } }
      }""",
      "explicitAcquisitionType skyOffset { p { arcseconds } q { arcseconds } }"
    ).flatMap: q =>
      expect(pi, q,
        json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gnirsImaging": {
                    "acquisition": {
                      "explicitAcquisitionType": "FAINT",
                      "skyOffset": {
                        "p": { "arcseconds": 1.500000 },
                        "q": { "arcseconds": -2.500000 }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight)

  test("create GNIRS imaging with an explicit acquisition filter"):
    createWithAcquisition("{ explicitFilter: ORDER4 }", "explicitFilter").flatMap: q =>
      expect(pi, q,
        json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gnirsImaging": {
                    "acquisition": { "explicitFilter": "ORDER4" }
                  }
                }
              }
            }
          }
        """.asRight)

  test("create GNIRS imaging with an explicit acquisition exposure time mode and coadds"):
    createWithAcquisition(
      """{
        explicitExposureTimeMode: {
          timeAndCount: { time: { seconds: 12.0 }, count: 1, at: { nanometers: 1250.0 } }
        }
        coadds: 4
      }""",
      "coadds exposureTimeMode { timeAndCount { time { seconds } count at { nanometers } } }"
    ).flatMap: q =>
      expect(pi, q,
        json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gnirsImaging": {
                    "acquisition": {
                      "coadds": 4,
                      "exposureTimeMode": {
                        "timeAndCount": {
                          "time": { "seconds": 12.000000 },
                          "count": 1,
                          "at": { "nanometers": 1250.000 }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """.asRight)

  test("create GNIRS imaging rejects a non-acquisition explicit filter"):
    createWithAcquisition("{ explicitFilter: Y }", "explicitFilter").flatMap: q =>
      expect(pi, q,
        List("Argument 'input.SET.observingMode.gnirsImaging.acquisition' is invalid: 'explicitFilter' must contain one of: ORDER6, ORDER5, ORDER4, H2, ORDER3, PAH").asLeft)

  test("create GNIRS imaging rejects a sky offset without FAINT acquisition type"):
    createWithAcquisition(
      """{
        explicitAcquisitionType: BRIGHT
        skyOffset: { p: { arcseconds: 1.5 }, q: { arcseconds: -2.5 } }
      }""",
      "explicitAcquisitionType"
    ).flatMap: q =>
      expect(pi, q,
        List("Argument 'input.SET.observingMode.gnirsImaging.acquisition' is invalid: 'skyOffset' is only valid when 'explicitAcquisitionType' is FAINT.").asLeft)

  test("create GNIRS imaging rejects FAINT acquisition type without a sky offset"):
    createWithAcquisition("{ explicitAcquisitionType: FAINT }", "explicitAcquisitionType").flatMap: q =>
      expect(pi, q,
        List("Argument 'input.SET.observingMode.gnirsImaging.acquisition' is invalid: 'explicitAcquisitionType' FAINT requires a 'skyOffset'.").asLeft)

  test("create GNIRS imaging with an explicit BRIGHT acquisition type and no sky offset"):
    createWithAcquisition("{ explicitAcquisitionType: BRIGHT }", "explicitAcquisitionType skyOffset { p { arcseconds } }").flatMap: q =>
      expect(pi, q,
        json"""
          {
            "createObservation": {
              "observation": {
                "observingMode": {
                  "gnirsImaging": {
                    "acquisition": {
                      "explicitAcquisitionType": "BRIGHT",
                      "skyOffset": null
                    }
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
