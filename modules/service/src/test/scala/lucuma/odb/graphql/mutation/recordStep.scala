// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.odb.data.ObservingModeType

class recordStep extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff)

  private def recordVisit(
    mode: ObservingModeType,
    user: User
  ): IO[(Program.Id, Observation.Id, Visit.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
    } yield (pid, oid, vid)

  private def recordStepTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Visit.Id => String,
    expected: Either[Visit.Id => String, Json]
  ): IO[Unit] =
    for {
      ids <- recordVisit(mode, user)
      (_, _, vid) = ids
      _   <- expect(user, query(vid), expected.leftMap(f => List(f(vid))))
    } yield ()

  test("recordStep - GmosNorth") {
    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigScience
          }) {
            stepRecord {
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
                  customMask {
                    filename
                    slitWidth
                  }
                  builtin
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "instrumentConfig": {
                "exposure": {
                  "seconds": 1200.000000
                },
                "readout": {
                  "xBin": "ONE",
                  "yBin": "ONE",
                  "ampCount": "TWELVE",
                  "ampGain": "LOW",
                  "ampReadMode": "SLOW"
                },
                "dtax": "TWO",
                "roi": "FULL_FRAME",
                "gratingConfig": {
                  "grating": "B1200_G5301",
                  "order": "ONE",
                  "wavelength": {
                    "nanometers": 600.000
                  }
                },
                "filter": null,
                "fpu": {
                  "customMask": null,
                  "builtin": "LONG_SLIT_0_50"
                }
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - GmosSouth") {
    recordStepTest(
      ObservingModeType.GmosSouthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosSouthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosSouth)},
            $stepConfigScience
          }) {
            stepRecord {
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
                  customMask {
                    filename
                    slitWidth
                  }
                  builtin
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosSouthStep": {
            "stepRecord": {
              "instrumentConfig": {
                "exposure": {
                  "seconds": 1200.000000
                },
                "readout": {
                  "xBin": "ONE",
                  "yBin": "ONE",
                  "ampCount": "TWELVE",
                  "ampGain": "LOW",
                  "ampReadMode": "SLOW"
                },
                "dtax": "TWO",
                "roi": "FULL_FRAME",
                "gratingConfig": {
                  "grating": "B1200_G5321",
                  "order": "ONE",
                  "wavelength": {
                    "nanometers": 600.000
                  }
                },
                "filter": null,
                "fpu": {
                  "customMask": null,
                  "builtin": "LONG_SLIT_0_50"
                }
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - mix up") {
    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosSouthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosSouth)},
            $stepConfigScience
          }) {
            stepRecord {
              instrumentConfig {
                exposure {
                  seconds
                }
              }
            }
          }
        }
      """,
      ((vid: Visit.Id) => s"Visit '$vid' not found or is not a GMOS South visit").asLeft
    )
  }

  test("recordStep - no grating") {
    val instrumentNoGrating: String =
    """
      instrument: {
        exposure: {
          seconds: 1200
        },
        readout: {
          xBin: ONE,
          yBin: ONE,
          ampCount: TWELVE,
          ampGain: LOW,
          ampReadMode: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME,
        fpu: {
          builtin: LONG_SLIT_0_50
        }
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            $instrumentNoGrating,
            $stepConfigScience
          }) {
            stepRecord {
              instrumentConfig {
                exposure {
                  seconds
                }
                gratingConfig {
                  grating
                  order
                  wavelength {
                    nanometers
                  }
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "instrumentConfig": {
                "exposure": {
                  "seconds": 1200.000000
                },
                "gratingConfig": null
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - no fpu") {
    val instrumentNoGrating: String =
    """
      instrument: {
        exposure: {
          seconds: 1200
        },
        readout: {
          xBin: ONE,
          yBin: ONE,
          ampCount: TWELVE,
          ampGain: LOW,
          ampReadMode: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            $instrumentNoGrating,
            $stepConfigScience
          }) {
            stepRecord {
              instrumentConfig {
                exposure {
                  seconds
                }
                fpu {
                  builtin
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "instrumentConfig": {
                "exposure": {
                  "seconds": 1200.000000
                },
                "fpu": null
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - custom mask") {
    val instrumentNoGrating: String =
    """
      instrument: {
        exposure: {
          seconds: 1200
        },
        readout: {
          xBin: ONE,
          yBin: ONE,
          ampCount: TWELVE,
          ampGain: LOW,
          ampReadMode: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME,
        fpu: {
          customMask: {
            filename: "foo",
            slitWidth: CUSTOM_WIDTH_0_75
          }
        }
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            $instrumentNoGrating,
            $stepConfigScience
          }) {
            stepRecord {
              instrumentConfig {
                exposure {
                  seconds
                }
                fpu {
                  customMask {
                    filename
                    slitWidth
                  }
                  builtin
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "instrumentConfig": {
                "exposure": {
                  "seconds": 1200.000000
                },
                "fpu": {
                  "customMask": {
                    "filename": "foo",
                    "slitWidth": "CUSTOM_WIDTH_0_75"
                  },
                  "builtin": null
                }
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - stepConfig Bias") {
    val stepConfigBias: String =
    """
      stepConfig: {
        bias: true
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigBias
          }) {
            stepRecord {
              stepConfig {
                stepType
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "stepConfig": {
                "stepType": "BIAS"
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - stepConfig Dark") {
    val stepConfigDark: String =
    """
      stepConfig: {
        dark: true
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigDark
          }) {
            stepRecord {
              stepConfig {
                stepType
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "stepConfig": {
                "stepType": "DARK"
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - stepConfig Gcal") {
    val stepConfigGcal: String =
    """
      stepConfig: {
        gcal: {
          arcs: [ AR_ARC, XE_ARC],
          filter: GMOS,
          diffuser: IR,
          shutter: OPEN
        }
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigGcal
          }) {
            stepRecord {
              stepConfig {
                stepType
                ... on Gcal {
                  arcs
                  filter
                  diffuser
                  shutter
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "stepConfig": {
                "stepType": "GCAL",
                "arcs": [ "AR_ARC", "XE_ARC" ],
                "filter": "GMOS",
                "diffuser": "IR",
                "shutter": "OPEN"
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - stepConfig Science") {
    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigScience
          }) {
            stepRecord {
              stepConfig {
                stepType
                ... on Science {
                  offset {
                    p {
                      arcseconds
                    }
                    q {
                      arcseconds
                    }
                  }
                  guiding
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "stepConfig": {
                "stepType": "SCIENCE",
                "offset": {
                  "p": {
                    "arcseconds": 0
                  },
                  "q": {
                    "arcseconds": 10
                  }
                },
                "guiding":  "ENABLED"
              }
            }
          }
        }
      """.asRight
    )
  }

  test("recordStep - stepConfig SmartGcal") {
    val stepConfigSmartGcal: String =
    """
      stepConfig: {
        smartGcal: {
          smartGcalType: FLAT
        }
      }
    """

    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigSmartGcal
          }) {
            stepRecord {
              stepConfig {
                stepType
                ... on SmartGcal {
                  smartGcalType
                }
              }
            }
          }
        }
      """,
      json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "stepConfig": {
                "stepType": "SMART_GCAL",
                "smartGcalType": "FLAT"
              }
            }
          }
        }
      """.asRight
    )
  }

}
