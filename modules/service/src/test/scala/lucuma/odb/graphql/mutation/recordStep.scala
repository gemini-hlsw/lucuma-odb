// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.bifunctor.*
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.*
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
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

  val instrumentGmosNorth: String =
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
          ampRead: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME,
        gratingConfig: {
          grating: B1200_G5301,
          order: ONE,
          wavelength: {
            nanometers: 600
          }
        },
        fpu: {
          builtin: LONG_SLIT_0_50
        }
      }
    """

  val instrumentGmosSouth: String =
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
          ampRead: SLOW
        },
        dtax: TWO,
        roi: FULL_FRAME,
        gratingConfig: {
          grating: B1200_G5321,
          order: ONE,
          wavelength: {
            nanometers: 600
          }
        },
        fpu: {
          builtin: LONG_SLIT_0_50
        }
      }
    """

  val stepConfigScience: String =
    """
      stepConfig: {
        science: {
          offset: {
             p: {
               arcseconds: 0
             },
             q: {
               arcseconds: 10
             }
          },
          guiding: ENABLED
        }
      }
    """

  test("recordStep - GmosNorth") {
    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      staff,
      vid => s"""
        mutation {
          recordGmosNorthStep(input: {
            visitId: ${vid.asJson},
            $instrumentGmosNorth,
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
            $instrumentGmosSouth,
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
            $instrumentGmosSouth,
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
          ampRead: SLOW
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
          ampRead: SLOW
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
          ampRead: SLOW
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
}
