// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
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
            filters { filter coadds }
            initialFilters { filter coadds }
            camera
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
                  "explicitWellDepth": null
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)

  // Sets a GNIRS imaging mode with a J science filter and an S/N science ETM at 1250nm.
  private val setGnirsImaging = """
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
      }
    }
  """

  private val acquisitionQuery = """
    observations {
      observingMode {
        gnirsImaging {
          acquisition {
            explicitAcquisitionType
            explicitFilter
            coadds
            skyOffset { p { arcseconds } q { arcseconds } }
            exposureTimeMode {
              signalToNoise { value at { nanometers } }
              timeAndCount { time { seconds } count at { nanometers } }
            }
          }
        }
      }
    }
  """

  private def acquisitionExpected(acquisition: io.circe.Json) =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "observingMode": {
                "gnirsImaging": { "acquisition": $acquisition }
              }
            }
          ]
        }
      }
    """.asRight

  test("observing mode: update GNIRS imaging acquisition type and sky offset"):
    val update = """
      observingMode: {
        gnirsImaging: {
          acquisition: {
            explicitAcquisitionType: FAINT
            skyOffset: { p: { arcseconds: 0.0 }, q: { arcseconds: 12.0 } }
          }
        }
      }
    """

    multiUpdateTest(pi,
      List(
        (setGnirsImaging, acquisitionQuery, acquisitionExpected(json"""
          {
            "explicitAcquisitionType": null,
            "explicitFilter": null,
            "coadds": 1,
            "skyOffset": null,
            "exposureTimeMode": {
              "signalToNoise": { "value": 10.000, "at": { "nanometers": 1250.000 } },
              "timeAndCount": null
            }
          }
        """)),
        (update, acquisitionQuery, acquisitionExpected(json"""
          {
            "explicitAcquisitionType": "FAINT",
            "explicitFilter": null,
            "coadds": 1,
            "skyOffset": {
              "p": { "arcseconds": 0.000000 },
              "q": { "arcseconds": 12.000000 }
            },
            "exposureTimeMode": {
              "signalToNoise": { "value": 10.000, "at": { "nanometers": 1250.000 } },
              "timeAndCount": null
            }
          }
        """))
      )
    )

  test("observing mode: clearing the GNIRS imaging acquisition type clears the sky offset"):
    val setFaint = """
      observingMode: {
        gnirsImaging: {
          acquisition: {
            explicitAcquisitionType: FAINT
            skyOffset: { p: { arcseconds: 0.0 }, q: { arcseconds: 12.0 } }
          }
        }
      }
    """

    val clear = """
      observingMode: {
        gnirsImaging: {
          acquisition: { explicitAcquisitionType: null }
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gnirsImaging {
            acquisition {
              explicitAcquisitionType
              skyOffset { p { arcseconds } q { arcseconds } }
            }
          }
        }
      }
    """

    multiUpdateTest(pi,
      List(
        (setGnirsImaging, query, acquisitionExpected(json"""
          { "explicitAcquisitionType": null, "skyOffset": null }
        """)),
        (setFaint, query, acquisitionExpected(json"""
          {
            "explicitAcquisitionType": "FAINT",
            "skyOffset": { "p": { "arcseconds": 0.000000 }, "q": { "arcseconds": 12.000000 } }
          }
        """)),
        (clear, query, acquisitionExpected(json"""
          { "explicitAcquisitionType": null, "skyOffset": null }
        """))
      )
    )

  test("observing mode: update GNIRS imaging acquisition filter and exposure time mode"):
    val update = """
      observingMode: {
        gnirsImaging: {
          acquisition: {
            explicitFilter: H2
            explicitExposureTimeMode: {
              timeAndCount: { time: { seconds: 8.0 }, count: 1, at: { nanometers: 1250.0 } }
            }
            coadds: 3
          }
        }
      }
    """

    multiUpdateTest(pi,
      List(
        (setGnirsImaging, acquisitionQuery, acquisitionExpected(json"""
          {
            "explicitAcquisitionType": null,
            "explicitFilter": null,
            "coadds": 1,
            "skyOffset": null,
            "exposureTimeMode": {
              "signalToNoise": { "value": 10.000, "at": { "nanometers": 1250.000 } },
              "timeAndCount": null
            }
          }
        """)),
        (update, acquisitionQuery, acquisitionExpected(json"""
          {
            "explicitAcquisitionType": null,
            "explicitFilter": "H2",
            "coadds": 3,
            "skyOffset": null,
            "exposureTimeMode": {
              "signalToNoise": null,
              "timeAndCount": {
                "time": { "seconds": 8.000000 },
                "count": 1,
                "at": { "nanometers": 1250.000 }
              }
            }
          }
        """))
      )
    )

  test("observing mode: editing the GNIRS imaging filters preserves the acquisition ETM"):
    val setWithAcqEtm = """
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
          acquisition: {
            explicitExposureTimeMode: {
              timeAndCount: { time: { seconds: 8.0 }, count: 1, at: { nanometers: 1250.0 } }
            }
            coadds: 3
          }
        }
      }
    """

    // Replacing the filter list must not reset the acquisition ETM back to its default.
    val editFilters = """
      observingMode: {
        gnirsImaging: {
          filters: [ { filter: K }, { filter: ORDER4 } ]
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gnirsImaging {
            filters { filter }
            acquisition {
              coadds
              exposureTimeMode {
                timeAndCount { time { seconds } count at { nanometers } }
              }
            }
          }
        }
      }
    """

    def expected(filters: io.circe.Json) =
      json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "gnirsImaging": {
                    "filters": $filters,
                    "acquisition": {
                      "coadds": 3,
                      "exposureTimeMode": {
                        "timeAndCount": {
                          "time": { "seconds": 8.000000 },
                          "count": 1,
                          "at": { "nanometers": 1250.000 }
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
        (setWithAcqEtm, query, expected(json"""[ { "filter": "J" } ]""")),
        (editFilters, query, expected(json"""[ { "filter": "ORDER4" }, { "filter": "K" } ]"""))
      )
    )

  test("observing mode: update GNIRS imaging per-filter coadds"):
    // The initial filters keep the coadds they were created with, and a
    // signal-to-noise filter is forced to 1 coadd.
    val update = """
      observingMode: {
        gnirsImaging: {
          filters: [
            {
              filter: J
              exposureTimeMode: {
                timeAndCount: { time: { seconds: 30.0 }, count: 6, at: { nanometers: 1250.0 } }
              }
              coadds: 5
            },
            {
              filter: ORDER4
              exposureTimeMode: {
                signalToNoise: { value: 50.0, at: { nanometers: 1250.0 } }
              }
              coadds: 5
            }
          ]
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gnirsImaging {
            filters { filter coadds }
            initialFilters { filter coadds }
          }
        }
      }
    """

    def expected(filters: io.circe.Json) =
      json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "gnirsImaging": {
                    "filters": $filters,
                    "initialFilters": [ { "filter": "J", "coadds": 1 } ]
                  }
                }
              }
            ]
          }
        }
      """.asRight

    multiUpdateTest(pi,
      List(
        (setGnirsImaging, query, expected(json"""[ { "filter": "J", "coadds": 1 } ]""")),
        (update, query, expected(json"""
          [ { "filter": "ORDER4", "coadds": 1 }, { "filter": "J", "coadds": 5 } ]
        """))
      )
    )

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
