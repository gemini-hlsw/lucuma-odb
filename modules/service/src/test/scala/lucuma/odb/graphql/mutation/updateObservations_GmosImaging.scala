// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GmosBinning
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ImageQuality
import lucuma.core.model.Observation
import lucuma.core.model.User
import lucuma.core.syntax.string.*

class updateObservations_GmosImaging extends OdbSuite with UpdateObservationsOps:

  val pi: User    = TestUsers.Standard.pi(nextId, nextId)
  val pi2: User   = TestUsers.Standard.pi(nextId, nextId)
  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi, pi2, staff)

  test("observing mode: set GMOS North imaging in existing observation"):
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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME },
            { filter: I_PRIME }
          ]
          variant: {
            interleaved: {
            }
          }
          explicitBin: TWO
          explicitAmpReadMode: SLOW
          explicitAmpGain: LOW
          explicitRoi: FULL_FRAME
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              interleaved {
                skyCount
              }
            }
            bin
            ampReadMode
            ampGain
            roi
          }
        }
      }
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" },
                    { "filter": "I_PRIME" }
                  ],
                  "variant": {
                    "interleaved": {
                      "skyCount": 0
                    }
                  },
                  "bin": "TWO",
                  "ampReadMode": "SLOW",
                  "ampGain": "LOW",
                  "roi": "FULL_FRAME"
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)

  test("observing mode: set GMOS South imaging in existing observation"):
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
        gmosSouthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME },
            { filter: I_PRIME }
          ]
          variant: {
            interleaved: {
              skyCount: 3
            }
          }
          explicitBin: FOUR
          explicitAmpReadMode: FAST
          explicitAmpGain: HIGH
          explicitRoi: CCD2
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosSouthImaging {
            filters { filter }
            variant {
              interleaved {
                skyCount
              }
            }
            bin
            ampReadMode
            ampGain
            roi
          }
        }
      }
    """

    val expected = json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_SOUTH",
              "observingMode": {
                "gmosSouthImaging": {
                  "filters":  [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" },
                    { "filter": "I_PRIME" }
                  ],
                  "variant": {
                    "interleaved": {
                      "skyCount": 3
                    }
                  },
                  "bin": "FOUR",
                  "ampReadMode": "FAST",
                  "ampGain": "HIGH",
                  "roi": "CCD2"
                }
              }
            }
          ]
        }
      }
    """.asRight

    oneUpdateTest(pi, update, query, expected)

  test("observing mode: update existing GMOS imaging"):

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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME }
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
          explicitBin: ONE
          explicitAmpReadMode: SLOW
          explicitAmpGain: LOW
          explicitRoi: FULL_FRAME
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              grouped {
                offsets {
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
            bin
            ampReadMode
            ampGain
            roi
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": {
                      "offsets": {
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
                        "random": {
                          "size": { "arcseconds": 14 },
                          "center": {
                            "p": { "arcseconds": 15 },
                            "q": { "arcseconds": 16 }
                          }
                        }
                      }
                    }
                  },
                  "bin": "ONE",
                  "ampReadMode": "SLOW",
                  "ampGain": "LOW",
                  "roi": "FULL_FRAME"
                }
              }
            }
          ]
        }
      }
    """.asRight

    val update1 = """
      observingMode: {
        gmosNorthImaging: {
          filters: [
            { filter: I_PRIME },
            { filter: Z_PRIME }
          ]
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
          explicitBin: FOUR
          explicitAmpReadMode: FAST
          explicitAmpGain: HIGH
          explicitRoi: CCD2
        }
      }
    """

    val expected1 =
      json"""
      {
        "updateObservations": {
          "observations": [
            {
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "I_PRIME" },
                    { "filter": "Z_PRIME" }
                  ],
                  "variant": {
                    "grouped": {
                      "offsets": {
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
                      "skyOffsets": null
                    }
                  },
                  "bin": "FOUR",
                  "ampReadMode": "FAST",
                  "ampGain": "HIGH",
                  "roi": "CCD2"
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

  test("observing mode: switch from GMOS imaging to long slit"):

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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME }
          ]
          variant: {
            interleaved: {
            }
          }
        }
      }
    """

    val query0 = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              interleaved {
                skyCount
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "interleaved": {
                      "skyCount": 0
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
        gmosNorthLongSlit: {
          grating: B1200_G5301
          filter: G_PRIME
          fpu: LONG_SLIT_0_25
          centralWavelength: {
            nanometers: 500
          }
          exposureTimeMode: {
            signalToNoise: {
              value: 20.0
              at: { nanometers: 500.0 }
            }
          }
        }
      }
    """

    val query1 = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              interleaved {
                skyCount
              }
            }
          }
          gmosNorthLongSlit {
            grating
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": null,
                "gmosNorthLongSlit": {
                  "grating": "B1200_G5301"
                }
              }
            }
          ]
        }
      }
    """.asRight

    multiUpdateTest(pi, List((update0, query0, expected0), (update1, query1, expected1)))

  test("observing mode: grouped -> interleaved -> grouped"):

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              grouped {
                order
                offsets {
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
                  random {
                    size { arcseconds }
                    center {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
              preImaging {
                offset1 {
                  p { arcseconds }
                  q { arcseconds }
                }
                offset2 {
                  p { arcseconds }
                  q { arcseconds }
                }
                offset3 {
                  p { arcseconds }
                  q { arcseconds }
                }
                offset4 {
                  p { arcseconds }
                  q { arcseconds }
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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME }
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": {
                      "order": "DECREASING",
                      "offsets": {
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
                        "random": {
                          "size": { "arcseconds": 13 },
                          "center": {
                            "p": { "arcseconds": 14 },
                            "q": { "arcseconds": 15 }
                          }
                        }
                      }
                    },
                    "interleaved": null,
                    "preImaging": null
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
        gmosNorthImaging: {
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": null,
                    "interleaved": {
                      "offsets": null,
                      "skyCount": 0,
                      "skyOffsets": null
                    },
                    "preImaging": null
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
        gmosNorthImaging: {
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": {
                      "order": "INCREASING",
                      "offsets": null,
                      "skyCount": 0,
                      "skyOffsets": null
                    },
                    "interleaved": null,
                    "preImaging": null
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

  test("observing mode: preImaging -> interleaved -> preImaging"):

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              grouped {
                order
                offsets {
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
                  random {
                    size { arcseconds }
                    center {
                      p { arcseconds }
                      q { arcseconds }
                    }
                  }
                }
              }
              preImaging {
                offset1 {
                  p { arcseconds }
                  q { arcseconds }
                }
                offset2 {
                  p { arcseconds }
                  q { arcseconds }
                }
                offset3 {
                  p { arcseconds }
                  q { arcseconds }
                }
                offset4 {
                  p { arcseconds }
                  q { arcseconds }
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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME }
          ]
          variant: {
            preImaging: {
              offset1: {
                p: { arcseconds: 10 }
                q: { arcseconds: 11 }
              }
              offset2: {
                p: { arcseconds: 12 }
                q: { arcseconds: 13 }
              }
              offset3: {
                p: { arcseconds: 14 }
                q: { arcseconds: 15 }
              }
              offset4: {
                p: { arcseconds: 16 }
                q: { arcseconds: 17 }
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": null,
                    "interleaved": null,
                    "preImaging": {
                      "offset1": {
                        "p": { "arcseconds": 10 },
                        "q": { "arcseconds": 11 }
                      },
                      "offset2": {
                        "p": { "arcseconds": 12 },
                        "q": { "arcseconds": 13 }
                      },
                      "offset3": {
                        "p": { "arcseconds": 14 },
                        "q": { "arcseconds": 15 }
                      },
                      "offset4": {
                        "p": { "arcseconds": 16 },
                        "q": { "arcseconds": 17 }
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
        gmosNorthImaging: {
          variant: {
            interleaved: {
              offsets: {
                spiral: {
                  size: { arcseconds: 20 }
                  center: {
                    p: { arcseconds: 21 }
                    q: { arcseconds: 22 }
                  }
                }
              }
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": null,
                    "interleaved": {
                      "offsets": {
                        "spiral": {
                          "size": { "arcseconds":  20},
                          "center": {
                            "p": { "arcseconds":  21 },
                            "q": { "arcseconds":  22 }
                          }
                        }
                      },
                      "skyCount": 0,
                      "skyOffsets": null
                    },
                    "preImaging": null
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
        gmosNorthImaging: {
          variant: {
            preImaging: {
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": null,
                    "interleaved": null,
                    "preImaging": {
                      "offset1": {
                        "p": { "arcseconds": 0 },
                        "q": { "arcseconds": 0 }
                      },
                      "offset2": {
                        "p": { "arcseconds": 0 },
                        "q": { "arcseconds": 0 }
                      },
                      "offset3": {
                        "p": { "arcseconds": 0 },
                        "q": { "arcseconds": 0 }
                      },
                      "offset4": {
                        "p": { "arcseconds": 0 },
                        "q": { "arcseconds": 0 }
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

    multiUpdateTest(
      pi,
      List(
        (update0, query, expected0), // initial preImaging
        (update1, query, expected1), // interleaved
        (update2, query, expected2)  // return to preImaging
      )
    )

  test("observing mode: update variant without setting filters"):

    val query = """
      observations {
        instrument
        observingMode {
          gmosNorthImaging {
            filters { filter }
            variant {
              grouped {
                order
                skyCount
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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME }
          ]
          variant: {
            grouped: {
              order: INCREASING
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": {
                      "order": "INCREASING",
                      "skyCount": 2
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
        gmosNorthImaging: {
          variant: {
            grouped: {
              skyCount: 3
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
                  ],
                  "variant": {
                    "grouped": {
                      "order": "INCREASING",
                      "skyCount": 3
                    }
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
        (update0, query, expected0),
        (update1, query, expected1)
      )
    )

  test("observing mode: delete GMOS imaging"):

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
        gmosNorthImaging: {
          filters: [
            { filter: G_PRIME },
            { filter: R_PRIME }
          ]
          variant: {
            interleaved: {
            }
          }
        }
      }
    """

    val query = """
      observations {
        instrument
        observingMode {
          mode
          gmosNorthImaging {
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
              "instrument": "GMOS_NORTH",
              "observingMode": {
                "mode": "GMOS_NORTH_IMAGING",
                "gmosNorthImaging": {
                  "filters": [
                    { "filter": "G_PRIME" },
                    { "filter": "R_PRIME" }
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

  test("observing mode: update GMOS imaging binning without filters"):

    val update = """
      observingMode: {
        gmosSouthImaging: {
          explicitBin: FOUR
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gmosSouthImaging {
            filters { filter }
            explicitBin
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
                  "gmosSouthImaging": {
                    "filters": [
                      { "filter": "G_PRIME" },
                      { "filter": "R_PRIME" }
                    ],
                    "explicitBin": "FOUR"
                  }
                }
              }
            ]
          }
        }
      """.asRight
    oneUpdateTest(pi, update, query, expected, ObservingModeType.GmosSouthImaging.some)


  test("observing mode: update default GMOS imaging bin by changing IQ"):
    val gaussian: String = """
      sourceProfile: {
        gaussian: {
          fwhm: { arcseconds: 0.1 }
          spectralDefinition: {
            bandNormalized: {
              sed: {
                stellarLibrary: B5_III
              }
              brightnesses: [
                {
                  band: R
                  value: 15.0
                  units: VEGA_MAGNITUDE
                }
              ]
            }
          }
        }
      }
    """

    def expectBinning(o: Observation.Id, b: GmosBinning): IO[Unit] =
      expect(
        user  = pi,
        query = s"""
            query {
              observation(observationId: "$o") {
                observingMode {
                  gmosNorthImaging {
                    bin
                  }
                }
              }
            }
        """,
        json"""
          {
            "observation": {
              "observingMode": {
                "gmosNorthImaging": {
                  "bin": ${b.tag.toScreamingSnakeCase}
                }
              }
            }
          }
        """.asRight
      )

    val update: String = """
      constraintSet: {
        imageQuality: POINT_ONE
      }
    """

    val throwawayQuery = """
      observations {
        observingMode {
          gmosSouthImaging {
            defaultBin
          }
        }
      }
    """

    for
      p <- createProgramAs(pi)
      t <- createTargetAs(pi, p, sourceProfile = gaussian)
      o <- createGmosNorthImagingObservationAs(pi, p, iq = ImageQuality.Preset.OnePointZero, t)
      _ <- expectBinning(o, GmosBinning.Two)
      _ <- query(pi, updateObservationsMutation(o, update, throwawayQuery))
      _ <- expectBinning(o, GmosBinning.One)
    yield ()


  test("observing mode: (fail to) update GMOS imaging with empty filters"):

    val update = """
      observingMode: {
        gmosNorthImaging: {
          filters: []
        }
      }
    """

    val query = """
      observations {
        observingMode {
          gmosNorthImaging {
            filters { filter }
          }
        }
      }
    """

    val expected = "Argument 'input.SET.observingMode.gmosNorthImaging' is invalid: At least one filter must be specified for GMOS imaging observations.".asLeft
    oneUpdateTest(pi, update, query, expected)

  test("observing mode: (fail to) update existing GMOS imaging with empty filters - rollback other changes"):
    createProgramAs(pi).flatMap: pid =>
      createGmosNorthImagingObservationAs(pi, pid).flatMap: oid =>
        val initialUpdate = """
          observingMode: {
            gmosNorthImaging: {
              filters: [
                { filter: G_PRIME },
                { filter: R_PRIME }
              ]
              variant: {
                interleaved: {
                  skyCount: 7
                }
              }
              explicitBin: TWO,
              explicitAmpGain: LOW
            }
          }
        """

        val failingUpdate = """
          observingMode: {
            gmosNorthImaging: {
              filters: []
              explicitBin: FOUR
              explicitAmpGain: HIGH
            }
          }
        """

        val query = """
          observations {
            observingMode {
              gmosNorthImaging {
                filters { filter }
                bin
                ampGain
              }
            }
          }
        """

        for
          _ <- expect(
            user = pi,
            query = updateObservationsMutation(oid, initialUpdate, query),
            expected = json"""
              {
                "updateObservations": {
                  "observations": [
                    {
                      "observingMode": {
                        "gmosNorthImaging": {
                          "filters": [
                            { "filter": "G_PRIME" },
                            { "filter": "R_PRIME" }
                          ],
                          "bin": "TWO",
                          "ampGain": "LOW"
                        }
                      }
                    }
                  ]
                }
              }
            """.asRight
          )
          _ <- expect(
            user = pi,
            query = updateObservationsMutation(oid, failingUpdate, query),
            expected = List("Argument 'input.SET.observingMode.gmosNorthImaging' is invalid: At least one filter must be specified for GMOS imaging observations.").asLeft
          )
          // Verify that ALL values remain unchanged (transaction rollback)
          _ <- expect(
            user = pi,
            query = s"""
              query {
                observation(observationId: "$oid") {
                  observingMode {
                    gmosNorthImaging {
                      filters { filter }
                      variant {
                        interleaved {
                          skyCount
                        }
                      }
                      initialFilters { filter }
                      bin
                      ampGain
                    }
                  }
                }
              }
            """,
            expected = json"""
              {
                "observation": {
                  "observingMode": {
                    "gmosNorthImaging": {
                      "filters": [
                        { "filter": "G_PRIME" },
                        { "filter": "R_PRIME" }
                      ],
                      "variant": {
                        "interleaved": {
                          "skyCount": 7
                        }
                      },
                      "initialFilters": [
                        { "filter": "G_PRIME" },
                        { "filter": "R_PRIME" }
                      ],
                      "bin": "TWO",
                      "ampGain": "LOW"
                    }
                  }
                }
              }
            """.asRight
          )
        yield ()