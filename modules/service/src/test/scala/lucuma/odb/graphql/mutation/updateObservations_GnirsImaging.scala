// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.model.User

class updateObservations_GnirsImaging extends OdbSuite with UpdateObservationsOps:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("observing mode: setting GNIRS imaging persists the mode"):
    val update = """
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
    """

    val query = """
      observations {
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
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
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
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)

  test("observing mode: update existing GNIRS imaging offsets"):

    val update0 = """
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
            { filter: J }
          ]
          variant: {
            grouped: {
              offsets: {
                enumerated: {
                  values: [
                    {
                      offset: {
                        p: { arcseconds: 10.0 }
                        q: { arcseconds: 11.0 }
                      }
                      guiding: ENABLED
                    },
                    {
                      offset: {
                        p: { arcseconds: 12.0 }
                        q: { arcseconds: 13.0 }
                      }
                      guiding: ENABLED
                    }
                  ]
                }
              }
              skyOffsets: {
                random: {
                  size: { arcseconds: 14.0 }
                  center: {
                    p: { arcseconds: 15.0 }
                    q: { arcseconds: 16.0 }
                  }
                }
              }
            }
          }
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gnirsImaging {
            filters { filter }
            variant {
              grouped {
                offsets {
                  generatorType
                  enumerated {
                    values {
                      offset {
                        p { arcseconds }
                        q { arcseconds }
                      }
                      guiding
                    }
                  }
                }
                skyOffsets {
                  generatorType
                  random {
                    size { arcseconds }
                    center {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    val expected0 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GNIRS",
              "observingMode": {
                "gnirsImaging": {
                  "filters": [
                    { "filter": "J" }
                  ],
                  "variant": {
                    "grouped": {
                      "offsets": {
                        "generatorType": "ENUMERATED",
                        "enumerated": {
                          "values": [
                            {
                              "offset": {
                                "p": { "arcseconds": 10 },
                                "q": { "arcseconds": 11 }
                              },
                              "guiding": "ENABLED"
                            },
                            {
                              "offset": {
                                "p": { "arcseconds": 12 },
                                "q": { "arcseconds": 13 }
                              },
                              "guiding": "ENABLED"
                            }
                          ]
                        }
                      },
                      "skyOffsets": {
                        "generatorType": "RANDOM",
                        "random": {
                          "size": { "arcseconds": 14 },
                          "center": {
                            "p": { "arcseconds": 15 },
                            "q": { "arcseconds": 16 }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        gnirsImaging: {
          variant: {
            grouped: {
              offsets: {
                enumerated: {
                  values: [
                    {
                      offset: {
                        p: { arcseconds: 17.0 }
                        q: { arcseconds: 18.0 }
                      }
                      guiding: ENABLED
                    }
                  ]
                }
              }
              skyOffsets: null
            }
          }
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GNIRS",
              "observingMode": {
                "gnirsImaging": {
                  "filters": [
                    { "filter": "J" }
                  ],
                  "variant": {
                    "grouped": {
                      "offsets": {
                        "generatorType": "ENUMERATED",
                        "enumerated": {
                          "values": [
                            {
                              "offset": {
                                "p": { "arcseconds": 17 },
                                "q": { "arcseconds": 18 }
                              },
                              "guiding": "ENABLED"
                            }
                          ]
                        }
                      },
                      "skyOffsets": {
                        "generatorType": "NONE",
                        "random": null
                      }
                    }
                  }
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi,
      List(
        (update0, query, expected0),
        (update1, query, expected1)
      )
    )
