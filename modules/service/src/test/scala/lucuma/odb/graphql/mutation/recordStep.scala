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
import lucuma.core.model.sequence.Atom
import lucuma.core.syntax.string.toScreamingSnakeCase
import lucuma.odb.data.ObservingModeType

class recordStep extends OdbSuite {

  val service: User = TestUsers.service(nextId)

  override lazy val validUsers: List[User] = List(service)

  private def recordVisitAndAtom(
    mode: ObservingModeType,
    user: User
  ): IO[(Program.Id, Observation.Id, Visit.Id, Atom.Id)] =
    for {
      pid <- createProgramAs(user)
      oid <- createObservationAs(user, pid, mode.some)
      vid <- recordVisitAs(user, mode.instrument, oid)
      aid <- recordAtomAs(user, mode.instrument, vid)
    } yield (pid, oid, vid, aid)

  private def recordStepTest(
    mode:     ObservingModeType,
    user:     User,
    query:    Atom.Id => String,
    expected: Atom.Id => Either[String, Json]
  ): IO[Unit] =
    for {
      ids <- recordVisitAndAtom(mode, user)
      (_, _, _, aid) = ids
      _   <- expect(user, query(aid), expected(aid).leftMap(msg => List(msg)))
    } yield ()

  test("recordStep - GmosNorth") {
    recordStepTest(
      ObservingModeType.GmosNorthLongSlit,
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              gmosNorth {
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
              observeClass
              estimate {
                seconds
              }
            }
          }
        }
      """,
      _ => json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "gmosNorth": {
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
              },
              "observeClass": "ACQUISITION",
              "estimate": {
                "seconds": 1299.562500
              }
            }
          }
        }
      """.asRight
    )

    // Step Time estimate:
    // 1200.0     Exposure
    //   82.5     Readout (HAMAMATSU, 1x1, 12 amp, Low gain, Slow read, FullFrame)
    //   10.0     Writeout
    //    7.0     Offset constant
    //    0.06250 Offset distance (0.00625 sec / arcsec X 10 arcsec)
    // ----------
    // 1299.56250 seconds

  }

  test("recordStep - GmosSouth") {
    recordStepTest(
      ObservingModeType.GmosSouthLongSlit,
      service,
      aid => s"""
        mutation {
          recordGmosSouthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosSouth)},
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              gmosSouth {
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
              },
              observeClass
              estimate {
                seconds
              }
            }
          }
        }
      """,
      _ => json"""
        {
          "recordGmosSouthStep": {
            "stepRecord": {
              "gmosSouth": {
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
              },
              "observeClass": "ACQUISITION",
              "estimate": {
                "seconds": 1299.562500
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
      service,
      aid => s"""
        mutation {
          recordGmosSouthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosSouth)},
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              gmosSouth {
                exposure {
                  seconds
                }
              }
            }
          }
        }
      """,
      aid => s"Atom '$aid' not found or is not a GMOS South atom".asLeft
    )
  }

  test("recordStep - no grating") {
    val instrumentNoGrating: String =
    """
      gmosNorth: {
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            $instrumentNoGrating,
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              gmosNorth {
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
      _ => json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "gmosNorth": {
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
      gmosNorth: {
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            $instrumentNoGrating,
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              gmosNorth {
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
      _ => json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "gmosNorth": {
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
      gmosNorth: {
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            $instrumentNoGrating,
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              gmosNorth {
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
      _ => json"""
        {
          "recordGmosNorthStep": {
            "stepRecord": {
              "gmosNorth": {
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigBias,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              stepConfig {
                stepType
              }
            }
          }
        }
      """,
      _ => json"""
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigDark,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
          }) {
            stepRecord {
              stepConfig {
                stepType
              }
            }
          }
        }
      """,
      _ => json"""
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigGcal,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
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
      _ => json"""
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigScience,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
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
      _ => json"""
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
      service,
      aid => s"""
        mutation {
          recordGmosNorthStep(input: {
            atomId: ${aid.asJson},
            ${dynamicConfig(Instrument.GmosNorth)},
            $stepConfigSmartGcal,
            observeClass: ${ObserveClass.Acquisition.tag.toScreamingSnakeCase}
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
      _ => json"""
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

  test("recordStep - with instrument config change cost") {
    val mode = ObservingModeType.GmosNorthLongSlit
    val cfg0 = dynamicConfig(mode.instrument)
    val cfg1 = cfg0.replaceFirst("builtin: LONG_SLIT_0_50", "builtin: LONG_SLIT_1_00")
    for {
      pid  <- createProgramAs(service)
      oid  <- createObservationAs(service, pid, mode.some)
      vid  <- recordVisitAs(service, mode.instrument, oid)
      aid  <- recordAtomAs(service, mode.instrument, vid)
      sid0 <- recordStepAs(service, aid, mode.instrument, cfg0, stepConfigScience)
      sid1 <- recordStepAs(service, aid, mode.instrument, cfg1, stepConfigScience)
      _    <- expect(service,
        s"""
          query {
            observation(observationId: "$oid") {
              execution {
                atomRecords {
                  matches {
                    steps {
                      matches {
                        estimate {
                          seconds
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
            "observation" : {
             "execution" : {
                "atomRecords" : {
                  "matches" : [
                    {
                      "steps" : {
                        "matches" : [
                          {
                            "estimate" : {
                              "seconds" : 1299.562500
                            }
                          },
                          {
                            "estimate" : {
                              "seconds" : 1352.500000
                            }
                          }
                        ]
                      }
                    }
                  ]
                }
              }
            }
          }
        """.asRight
      )
    } yield ()

    // Step Estimate 1:
    // 1200.0     Exposure
    //   82.5     Readout (HAMAMATSU, 1x1, 12 amp, Low gain, Slow read, FullFrame)
    //   10.0     Writeout
    //    7.0     Offset constant
    //    0.06250 Offset distance (0.00625 sec / arcsec X 10 arcsec)
    // ----------
    // 1299.562500 seconds

    // Step Estimate 2:
    // 1200.0     Exposure
    //   82.5     Readout (HAMAMATSU, 1x1, 12 amp, Low gain, Slow read, FullFrame)
    //   10.0     Writeout
    //   60.0     FPU change cost
    // ----------
    // 1352.500000 seconds

  }

}
