// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.User

class updateObservations_Flamingos2Imaging extends OdbSuite with UpdateObservationsOps:

  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("observing mode: setting Flamingos2 imaging persists the mode"):
    val update = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0
            at: { nanometers: 500.0 }
          }
        }
      }
      observingMode: {
        flamingos2Imaging: {
          filters: [
            { filter: Y },
            { filter: J }
          ]
          explicitReadMode: BRIGHT
          explicitDecker: IMAGING
          explicitReadoutMode: SCIENCE
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          mode
          flamingos2Imaging {
            filters { filter }
            initialFilters { filter }
            defaultReadMode
            explicitReadMode
            defaultReads
            explicitReads
            decker
            defaultDecker
            explicitDecker
            readoutMode
            defaultReadoutMode
            explicitReadoutMode
          }
        }
      }
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_IMAGING",
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "initialFilters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "defaultReadMode": "FAINT",
                  "explicitReadMode": "BRIGHT",
                  "defaultReads": "READS_1",
                  "explicitReads": null,
                  "decker": "IMAGING",
                  "defaultDecker": "IMAGING",
                  "explicitDecker": "IMAGING",
                  "readoutMode": "SCIENCE",
                  "defaultReadoutMode": "SCIENCE",
                  "explicitReadoutMode": "SCIENCE"
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)

  test("observing mode: delete Flamingos2 imaging"):

    val update0 = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0
            at: { nanometers: 500.0 }
          }
        }
      }
      observingMode: {
        flamingos2Imaging: {
          filters: [
            { filter: Y },
            { filter: J }
          ]
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          mode
          flamingos2Imaging {
            filters { filter }
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
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_IMAGING",
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ]
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: null
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": null,
              "observingMode": null
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

  test("observing mode: update existing F2 imaging filters and common config"):

    val update0 = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0
            at: { nanometers: 500.0 }
          }
        }
      }
      observingMode: {
        flamingos2Imaging: {
          filters: [
            { filter: Y },
            { filter: J }
          ]
          explicitReadMode: BRIGHT
          explicitDecker: IMAGING
          explicitReadoutMode: SCIENCE
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          mode
          flamingos2Imaging {
            filters { filter }
            initialFilters { filter }
            explicitReadMode
            explicitDecker
            explicitReadoutMode
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
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_IMAGING",
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "initialFilters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "explicitReadMode": "BRIGHT",
                  "explicitDecker": "IMAGING",
                  "explicitReadoutMode": "SCIENCE"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        flamingos2Imaging: {
          filters: [
            { filter: Y },
            { filter: H },
            { filter: K_SHORT }
          ]
          explicitReadMode: FAINT
          explicitReadoutMode: ENGINEERING
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "mode": "FLAMINGOS_2_IMAGING",
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "H" },
                    { "filter": "K_SHORT" }
                  ],
                  "initialFilters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "explicitReadMode": "FAINT",
                  "explicitDecker": "IMAGING",
                  "explicitReadoutMode": "ENGINEERING"
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

  test("observing mode: update existing F2 imaging offsets"):

    val update0 = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0
            at: { nanometers: 500.0 }
          }
        }
      }
      observingMode: {
        flamingos2Imaging: {
          filters: [
            { filter: Y },
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
          flamingos2Imaging {
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
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
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
        flamingos2Imaging: {
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
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
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

  test("observing mode: grouped -> interleaved -> grouped (F2 imaging)"):

    val query = """
      observations {
        instrument
        observingMode {
          flamingos2Imaging {
            filters { filter }
            variant {
              grouped {
                order
                offsets {
                  generatorType
                  spiral {
                    size { arcseconds }
                    center {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
                skyCount
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
              interleaved {
                offsets {
                  generatorType
                  spiral {
                    size { arcseconds }
                    center {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
                skyCount
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

    val update0 = """
      scienceRequirements: {
        exposureTimeMode: {
          signalToNoise: {
            value: 100.0
            at: { nanometers: 500.0 }
          }
        }
      }
      observingMode: {
        flamingos2Imaging: {
          filters: [
            { filter: Y },
            { filter: J }
          ]
          variant: {
            grouped: {
              order: DECREASING
              offsets: {
                spiral: {
                  size: { arcseconds: 10 }
                  center: {
                    p: { arcseconds: 11 }
                    q: { arcseconds: 12 }
                  }
                }
              }
              skyCount: 2
              skyOffsets: {
                random: {
                  size: { arcseconds: 13 }
                  center: {
                    p: { arcseconds: 14 }
                    q: { arcseconds: 15 }
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
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "variant": {
                    "grouped": {
                      "order": "DECREASING",
                      "offsets": {
                        "generatorType": "SPIRAL",
                        "spiral": {
                          "size": { "arcseconds": 10 },
                          "center": {
                            "p": { "arcseconds": 11 },
                            "q": { "arcseconds": 12 }
                          }
                        }
                      },
                      "skyCount": 2,
                      "skyOffsets": {
                        "generatorType": "RANDOM",
                        "random": {
                          "size": { "arcseconds": 13 },
                          "center": {
                            "p": { "arcseconds": 14 },
                            "q": { "arcseconds": 15 }
                          }
                        }
                      }
                    },
                    "interleaved": null
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
        flamingos2Imaging: {
          variant: {
            interleaved: {
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
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "variant": {
                    "grouped": null,
                    "interleaved": {
                      "offsets": {
                        "generatorType": "NONE",
                        "spiral": null
                      },
                      "skyCount": 0,
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

    val update2 = """
      observingMode: {
        flamingos2Imaging: {
          variant: {
            grouped: {
            }
          }
        }
      }
    """

    val expected2 =
    json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "FLAMINGOS2",
              "observingMode": {
                "flamingos2Imaging": {
                  "filters": [
                    { "filter": "Y" },
                    { "filter": "J" }
                  ],
                  "variant": {
                    "grouped": {
                      "order": "INCREASING",
                      "offsets": {
                        "generatorType": "NONE",
                        "spiral": null
                      },
                      "skyCount": 0,
                      "skyOffsets": {
                        "generatorType": "NONE",
                        "random": null
                      }
                    },
                    "interleaved": null
                  }
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(
      pi,
      List(
        (update0, query, expected0), // initial grouped
        (update1, query, expected1), // interleaved
        (update2, query, expected2)  // return to grouped
      )
    )

  test("observing mode: update F2 imaging readMode without filters"):

    val update = """
      observingMode: {
        flamingos2Imaging: {
          explicitReadMode: BRIGHT
        }
      }
    """

    val query = """
      observations {
        observingMode {
          flamingos2Imaging {
            filters { filter }
            explicitReadMode
          }
        }
      }
    """

    val expected =
      json"""
        {
          "updateObservations": {
            "observations": [
              {
                "observingMode": {
                  "flamingos2Imaging": {
                    "filters": [
                      { "filter": "Y" },
                      { "filter": "J" }
                    ],
                    "explicitReadMode": "BRIGHT"
                  }
                }
              }
            ]
          }
        }
      """.asRight
    oneUpdateTest(pi, update, query, expected, ObservingModeType.Flamingos2Imaging.some)
