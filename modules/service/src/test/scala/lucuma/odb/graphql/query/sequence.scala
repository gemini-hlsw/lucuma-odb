// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.SmartGcalType
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.service.Services
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

class sequence extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "Sequence Testing")

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val tableRow: TableRow.North =
      TableRow(
        PosLong.unsafeFrom(1),
        TableKey(
          GratingConfigKey(
            GmosNorthGrating.R831_G5302,
            GmosGratingOrder.One,
            BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
          ).some,
          GmosNorthFilter.RPrime.some,
          GmosNorthFpu.LongSlit_0_50.some,
          GmosXBinning.One,
          GmosYBinning.Two,
          GmosAmpGain.Low
        ),
        SmartGcalValue(
          Gcal(
            Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
            GcalFilter.Gmos,
            GcalDiffuser.Ir,
            GcalShutter.Open
          ),
          GcalBaselineType.Night,
          PosInt.unsafeFrom(1),
          LegacyInstrumentConfig(
            TimeSpan.unsafeFromMicroseconds(1_000_000L)
          )
        )
      )
    val services = Services.forUser(pi /* doesn't matter*/)(s)
    services.transactionally {
      services.smartGcalService.insertGmosNorth(1, tableRow)
    }
  }

  test("simple generation") {
    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, _) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     static {
                       stageMode
                       detector
                       mosPreImaging
                       nodAndShuffle {
                         posA { p { microarcseconds } }
                       }
                     }
                     acquisition {
                       digest {
                         observeClass
                       }
                       nextAtom {
                         observeClass
                         steps {
                           observeClass
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                             readout {
                               xBin
                               yBin
                             }
                             roi,
                             fpu {
                               builtin
                             }
                           }
                           stepConfig {
                             ... on Science {
                               offset {
                                 p { arcseconds }
                                 q { arcseconds }
                               }
                             }
                           }
                         }
                       }
                       possibleFuture {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig": {
                  "static": {
                    "stageMode": "FOLLOW_XY",
                    "detector": "HAMAMATSU",
                    "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
                    "nodAndShuffle": null
                  },
                  "acquisition": {
                    "digest": {
                      "observeClass": "ACQUISITION"
                    },
                    "nextAtom": {
                      "observeClass": "ACQUISITION",
                      "steps": [
                        {
                          "observeClass": "ACQUISITION",
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 10.000000
                            },
                            "readout": {
                              "xBin": "TWO",
                              "yBin": "TWO"
                            },
                            "roi": "CCD2",
                            "fpu": null
                          },
                          "stepConfig": {
                            "offset": {
                              "p": {
                                "arcseconds": 0.000000
                              },
                              "q": {
                                "arcseconds": 0.000000
                              }
                            }
                          }
                        },
                        {
                          "observeClass": "ACQUISITION",
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 20.000000
                            },
                            "readout": {
                              "xBin": "ONE",
                              "yBin": "ONE"
                            },
                            "roi": "CENTRAL_STAMP",
                            "fpu": {
                              "builtin": "LONG_SLIT_0_50"
                            }
                          },
                          "stepConfig": {
                            "offset": {
                              "p": {
                                "arcseconds": 10.000000
                              },
                              "q": {
                                "arcseconds": 0.000000
                              }
                            }
                          }
                        },
                        {
                          "observeClass": "ACQUISITION",
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 40.000000
                            },
                            "readout": {
                              "xBin": "ONE",
                              "yBin": "ONE"
                            },
                            "roi": "CENTRAL_STAMP",
                            "fpu": {
                              "builtin": "LONG_SLIT_0_50"
                            }
                          },
                          "stepConfig": {
                            "offset": {
                              "p": {
                                "arcseconds": 0.000000
                              },
                              "q": {
                                "arcseconds": 0.000000
                              }
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture": []
                  }
                }
              }
            }
          """
        )
      )
    }

  }

  test("simple generation - limited future") {
    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, _) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid", futureLimit: 1) {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       nextAtom {
                         observeClass
                       }
                       possibleFuture {
                         observeClass
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig": {
                  "science": {
                    "nextAtom": {
                      "observeClass": "SCIENCE"
                    },
                    "possibleFuture": [
                      {
                        "observeClass": "SCIENCE"
                      }
                    ]
                  }
                }
              }
            }
          """
        )
      )
    }

  }

  test("explicit offsets") {

    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitSpatialOffsets: [
                { arcseconds: -15.0 },
                { arcseconds:  15.0 },
                { arcseconds:  15.0 },
                { arcseconds: -15.0 }
              ]
            }
          """
        )
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, tid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       digest {
                         offsets {
                           q { arcseconds }
                         }
                       }
                       nextAtom {
                         description
                         steps {
                           stepConfig {
                             stepType
                             ... on Science {
                               offset {
                                 q { arcseconds }
                               }
                             }
                           }
                         }
                       }
                       possibleFuture {
                         description
                         steps {
                           stepConfig {
                             stepType
                             ... on Science {
                               offset {
                                 q { arcseconds }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig": {
                  "science": {
                    "digest": {
                      "offsets": [
                        {
                          "q": {
                            "arcseconds": -15.000000
                          }
                        },
                        {
                          "q": {
                            "arcseconds": 15.000000
                          }
                        }
                      ]
                    },
                    "nextAtom": {
                      "description": "q -15.0″, λ 500.0 nm",
                      "steps": [
                        {
                          "stepConfig": {
                            "stepType": "SCIENCE",
                            "offset": {
                              "q": {
                                "arcseconds": -15.000000
                              }
                            }
                          }
                        },
                        {
                          "stepConfig": {
                            "stepType": "GCAL"
                          }
                        }
                      ]
                    },
                    "possibleFuture": [
                      {
                        "description": "q 15.0″, λ 505.0 nm",
                        "steps": [
                          {
                            "stepConfig": {
                              "stepType": "GCAL"
                            }
                          },
                          {
                            "stepConfig": {
                              "stepType": "SCIENCE",
                              "offset": {
                                "q": {
                                  "arcseconds": 15.000000
                                }
                              }
                            }
                          }
                        ]
                      },
                      {
                        "description": "q 15.0″, λ 505.0 nm",
                        "steps": [
                          {
                            "stepConfig": {
                              "stepType": "SCIENCE",
                              "offset": {
                                "q": {
                                  "arcseconds": 15.000000
                                }
                              }
                            }
                          },
                          {
                            "stepConfig": {
                              "stepType": "GCAL"
                            }
                          }
                        ]
                      },
                      {
                        "description": "q -15.0″, λ 500.0 nm",
                        "steps": [
                          {
                            "stepConfig": {
                              "stepType": "GCAL"
                            }
                          },
                          {
                            "stepConfig": {
                              "stepType": "SCIENCE",
                              "offset": {
                                "q": {
                                  "arcseconds": -15.000000
                                }
                              }
                            }
                          }
                        ]
                      },
                      {
                        "description": "q -15.0″, λ 500.0 nm",
                        "steps": [
                          {
                            "stepConfig": {
                              "stepType": "SCIENCE",
                              "offset": {
                                "q": {
                                  "arcseconds": -15.000000
                                }
                              }
                            }
                          },
                          {
                            "stepConfig": {
                              "stepType": "GCAL"
                            }
                          }
                        ]
                      },
                      {
                        "description": "q 15.0″, λ 505.0 nm",
                        "steps": [
                          {
                            "stepConfig": {
                              "stepType": "GCAL"
                            }
                          },
                          {
                            "stepConfig": {
                              "stepType": "SCIENCE",
                              "offset": {
                                "q": {
                                  "arcseconds": 15.000000
                                }
                              }
                            }
                          }
                        ]
                      }
                    ]
                  }
                }
              }
            }
          """
        )
      )
    }

  }

  test("explicit wavelength dithers") {

    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitWavelengthDithers: [
                { nanometers: -5.0 },
                { nanometers:  0.0 },
                { nanometers:  5.0 }
              ]
            }
          """
        )
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, tid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       nextAtom {
                         description
                         steps {
                           instrumentConfig {
                             gratingConfig {
                               wavelength { nanometers }
                             }
                           }
                         }
                       }
                       possibleFuture {
                         description
                         steps {
                           instrumentConfig {
                             gratingConfig {
                               wavelength { nanometers }
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig": {
                  "science": {
                    "nextAtom": {
                      "description": "q 0.0″, λ 495.0 nm",
                      "steps": [
                        {
                          "instrumentConfig": {
                            "gratingConfig": {
                              "wavelength": {
                                "nanometers": 495.000
                              }
                            }
                          }
                        },
                        {
                          "instrumentConfig": {
                            "gratingConfig": {
                              "wavelength": {
                                "nanometers": 495.000
                              }
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture": [
                      {
                        "description": "q 15.0″, λ 500.0 nm",
                        "steps": [
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 500.000
                                }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 500.000
                                }
                              }
                            }
                          }
                        ]
                      },
                      {
                        "description": "q 15.0″, λ 505.0 nm",
                        "steps": [
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 505.000
                                }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 505.000
                                }
                              }
                            }
                          }
                        ]
                      },
                      {
                        "description": "q 0.0″, λ 495.0 nm",
                        "steps": [
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 495.000
                                }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 495.000
                                }
                              }
                            }
                          }

                        ]
                      },
                      {
                        "description": "q 0.0″, λ 500.0 nm",
                        "steps": [
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 500.000
                                }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 500.000
                                }
                              }
                            }
                          }
                        ]
                      },
                      {
                        "description": "q 15.0″, λ 505.0 nm",
                        "steps": [
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 505.000
                                }
                              }
                            }
                          },
                          {
                            "instrumentConfig": {
                              "gratingConfig": {
                                "wavelength": {
                                  "nanometers": 505.000
                                }
                              }
                            }
                          }
                        ]
                      }
                    ]
                  }
                }
              }
            }
          """
        )
      )
    }

  }

  test("user cannot access program") {
    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, _) =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 executionConfig {
                   instrument
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": null
            }
          """
        )
      )
    }

  }

  test("observation doesn't correspond to program") {
    val setup: IO[(Program.Id, Program.Id, Observation.Id, Target.Id)] =
      for {
        p0 <- createProgramAs(pi, "foo")
        p1 <- createProgram
        t  <- createTargetWithProfileAs(user, p1)
        o  <- createGmosNorthLongSlitObservationAs(user, p1, t)
      } yield (p0, p1, o, t)

    setup.flatMap { case (pid0, pid1, oid, _) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid0", observationId: "$oid") {
                 executionConfig {
                   instrument
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": null
            }
          """
        )
      )
    }

  }

  test("cross site execution config") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o)

    setup.flatMap { case (pid, oid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 executionConfig {
                   ... on GmosSouthExecutionConfig {
                     science {
                       nextAtom {
                         steps {
                           instrumentConfig {
                             gratingConfig {
                               wavelength { nanometers }
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "executionConfig": {
                }
              }
            }
          """
        )
      )
    }

  }


  test("planned time: config and detector estimates") {

    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, _) =>
      expect(
        user  = user,
        query =
          s"""
             fragment plannedTimeFields on PlannedTime {
               total {
                 seconds
               }
               charges {
                 chargeClass
                 time {
                   seconds
                 }
               }
             }

             fragment configChangeEstimateFields on ConfigChangeEstimate {
               name
               description
               estimate {
                 seconds
               }
             }

             fragment allConfigChangeEstimatesFields on AllConfigChangeEstimates {
               selected {
                 ...configChangeEstimateFields
               }
               all {
                 ...configChangeEstimateFields
               }
               estimate {
                 seconds
               }
             }

             fragment datasetEstimateFields on DatasetEstimate {
               estimate {
                 seconds
               }
               exposure {
                 seconds
               }
               readout {
                 seconds
               }
               write {
                 seconds
               }
             }

             fragment detectorEstimateFields on DetectorEstimate {
               name
               description
               dataset {
                 ...datasetEstimateFields
               }
               count
               estimate {
                 seconds
               }
             }

             fragment allDetectorEstimatesFields on AllDetectorEstimates {
               selected {
                 ...detectorEstimateFields
               }
               all {
                 ...detectorEstimateFields
               }
               estimate {
                 seconds
               }
             }

             fragment stepEstimateFields on StepEstimate {
               configChange {
                 ...allConfigChangeEstimatesFields
               }
               detector {
                 ...allDetectorEstimatesFields
               }
               total {
                 seconds
               }
             }

             fragment gmosNorthAtomFields on GmosNorthAtom {
               steps {
                 instrumentConfig {
                   readout {
                     xBin
                     yBin
                     ampCount
                     ampGain
                     ampReadMode
                   }
                   roi
                 }
                 estimate {
                   ...stepEstimateFields
                 }
               }
             }

             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     acquisition {
                       nextAtom {
                         ...gmosNorthAtomFields
                       }
                       possibleFuture {
                         ...gmosNorthAtomFields
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig": {
                  "acquisition": {
                    "nextAtom": {
                       "steps": [
                        {
                          "instrumentConfig" : {
                            "readout" : {
                              "xBin" : "TWO",
                              "yBin" : "TWO",
                              "ampCount" : "TWELVE",
                              "ampGain" : "LOW",
                              "ampReadMode" : "FAST"
                            },
                            "roi" : "CCD2"
                          },
                          "estimate": {
                            "configChange": null,
                            "detector": {
                              "selected": {
                                "name": "GMOS North",
                                "description": "GMOS North Hamamatsu Detector Array",
                                "dataset": {
                                  "estimate": {
                                    "seconds": 29.700000
                                  },
                                  "exposure": {
                                    "seconds": 10.000000
                                  },
                                  "readout": {
                                    "seconds": 9.700000
                                  },
                                  "write": {
                                    "seconds": 10.000000
                                  }
                                },
                                "count": 1,
                                "estimate": {
                                  "seconds": 29.700000
                                }
                              },
                              "all": [
                                {
                                  "name": "GMOS North",
                                  "description": "GMOS North Hamamatsu Detector Array",
                                  "dataset": {
                                    "estimate": {
                                      "seconds": 29.700000
                                    },
                                    "exposure": {
                                      "seconds": 10.000000
                                    },
                                    "readout": {
                                      "seconds": 9.700000
                                    },
                                    "write": {
                                      "seconds": 10.000000
                                    }
                                  },
                                  "count": 1,
                                  "estimate": {
                                    "seconds": 29.700000
                                  }
                                }
                              ],
                              "estimate": {
                                "seconds": 29.700000
                              }
                            },
                            "total": {
                              "seconds": 29.700000
                            }
                          }
                        },
                      {
                        "instrumentConfig" : {
                          "readout" : {
                            "xBin" : "ONE",
                            "yBin" : "ONE",
                            "ampCount" : "TWELVE",
                            "ampGain" : "LOW",
                            "ampReadMode" : "FAST"
                          },
                          "roi" : "CENTRAL_STAMP"
                        },
                        "estimate" : {
                          "configChange" : {
                            "selected" : {
                              "name" : "GMOS North FPU",
                              "description" : "GMOS North FPU change cost",
                              "estimate" : {
                                "seconds" : 60.000000
                              }
                            },
                            "all" : [
                              {
                                "name" : "GMOS North FPU",
                                "description" : "GMOS North FPU change cost",
                                "estimate" : {
                                  "seconds" : 60.000000
                                }
                              },
                              {
                                "name" : "Offset",
                                "description" : "Offset cost, 7 (constant) + 0.0625 (distance)",
                                "estimate" : {
                                  "seconds" : 7.062500
                                }
                              }
                            ],
                            "estimate" : {
                              "seconds" : 60.000000
                            }
                          },
                          "detector" : {
                            "selected" : {
                              "name" : "GMOS North",
                              "description" : "GMOS North Hamamatsu Detector Array",
                              "dataset" : {
                                "estimate" : {
                                  "seconds" : 34.200000
                                },
                                "exposure" : {
                                  "seconds" : 20.000000
                                },
                                "readout" : {
                                  "seconds" : 4.200000
                                },
                                "write" : {
                                  "seconds" : 10.000000
                                }
                              },
                              "count" : 1,
                              "estimate" : {
                                "seconds" : 34.200000
                              }
                            },
                            "all" : [
                              {
                                "name" : "GMOS North",
                                "description" : "GMOS North Hamamatsu Detector Array",
                                "dataset" : {
                                  "estimate" : {
                                    "seconds" : 34.200000
                                  },
                                  "exposure" : {
                                    "seconds" : 20.000000
                                  },
                                  "readout" : {
                                    "seconds" : 4.200000
                                  },
                                  "write" : {
                                    "seconds" : 10.000000
                                  }
                                },
                                "count" : 1,
                                "estimate" : {
                                  "seconds" : 34.200000
                                }
                              }
                            ],
                            "estimate" : {
                              "seconds" : 34.200000
                            }
                          },
                          "total" : {
                            "seconds" : 94.200000
                          }
                        }
                      },
                      {
                        "instrumentConfig" : {
                          "readout" : {
                            "xBin" : "ONE",
                            "yBin" : "ONE",
                            "ampCount" : "TWELVE",
                            "ampGain" : "LOW",
                            "ampReadMode" : "FAST"
                          },
                          "roi" : "CENTRAL_STAMP"
                        },
                        "estimate" : {
                          "configChange" : {
                            "selected" : {
                              "name" : "Offset",
                              "description" : "Offset cost, 7 (constant) + 0.0625 (distance)",
                              "estimate" : {
                                "seconds" : 7.062500
                              }
                            },
                            "all" : [
                              {
                                "name" : "Offset",
                                "description" : "Offset cost, 7 (constant) + 0.0625 (distance)",
                                "estimate" : {
                                  "seconds" : 7.062500
                                }
                              }
                            ],
                            "estimate" : {
                              "seconds" : 7.062500
                            }
                          },
                          "detector" : {
                            "selected" : {
                              "name" : "GMOS North",
                              "description" : "GMOS North Hamamatsu Detector Array",
                              "dataset" : {
                                "estimate" : {
                                  "seconds" : 54.200000
                                },
                                "exposure" : {
                                  "seconds" : 40.000000
                                },
                                "readout" : {
                                  "seconds" : 4.200000
                                },
                                "write" : {
                                  "seconds" : 10.000000
                                }
                              },
                              "count" : 1,
                              "estimate" : {
                                "seconds" : 54.200000
                              }
                            },
                            "all" : [
                              {
                                "name" : "GMOS North",
                                "description" : "GMOS North Hamamatsu Detector Array",
                                "dataset" : {
                                  "estimate" : {
                                    "seconds" : 54.200000
                                  },
                                  "exposure" : {
                                    "seconds" : 40.000000
                                  },
                                  "readout" : {
                                    "seconds" : 4.200000
                                  },
                                  "write" : {
                                    "seconds" : 10.000000
                                  }
                                },
                                "count" : 1,
                                "estimate" : {
                                  "seconds" : 54.200000
                                }
                              }
                            ],
                            "estimate" : {
                              "seconds" : 54.200000
                            }
                          },
                          "total" : {
                            "seconds" : 61.262500
                          }
                        }
                       }
                      ]
                    },
                    "possibleFuture": []
                  }
                }
              }
            }
          """
        )
      )
    }
  }

  test("planned time: observation level") {

    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o, t)

    setup.flatMap { case (pid, oid, _) =>
      expect(
        user  = user,
        query =
          s"""
             fragment plannedTimeFields on PlannedTime {
               total {
                 seconds
               }
               charges {
                 chargeClass
                 time {
                   seconds
                 }
               }
             }

             fragment stepEstimateFields on StepEstimate {
               total {
                 seconds
               }
             }

             fragment gmosNorthAtomFields on GmosNorthAtom {
               steps {
                 estimate {
                   ...stepEstimateFields
                 }
               }
             }

             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     setup {
                       full {
                         seconds
                       }
                       reacquisition {
                         seconds
                       }
                     }
                     science {
                       digest {
                         plannedTime {
                           ...plannedTimeFields
                         }
                       }
                       nextAtom {
                         ...gmosNorthAtomFields
                       }
                       possibleFuture {
                         ...gmosNorthAtomFields
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig" : {
                  "setup": {
                    "full": {
                      "seconds": 960.000000
                    },
                    "reacquisition": {
                      "seconds": 300.000000
                    }
                  },
                  "science" : {
                    "digest": {
                      "plannedTime" : {
                        "total" : {
                          "seconds" : 769.200000
                        },
                        "charges" : [
                          {
                            "chargeClass" : "NON_CHARGED",
                            "time" : {
                              "seconds" : 0.000000
                            }
                          },
                          {
                            "chargeClass" : "PARTNER",
                            "time" : {
                              "seconds" : 357.600000
                            }
                          },
                          {
                            "chargeClass" : "PROGRAM",
                            "time" : {
                              "seconds" : 411.600000
                            }
                          }
                        ]
                      }
                    },
                    "nextAtom" : {
                      "steps" : [
                        {
                          "estimate" : {
                            "total" : {
                              "seconds" : 61.100000
                            }
                          }
                        },
                        {
                          "estimate" : {
                            "total" : {
                              "seconds" : 67.100000
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture" : [
                      {
                        "steps" : [
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 52.100000
                              }
                            }
                          },
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 76.100000
                              }
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 61.100000
                              }
                            }
                          },
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 67.100000
                              }
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 52.100000
                              }
                            }
                          },
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 76.100000
                              }
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 61.100000
                              }
                            }
                          },
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 67.100000
                              }
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 52.100000
                              }
                            }
                          },
                          {
                            "estimate" : {
                              "total" : {
                                "seconds" : 76.100000
                              }
                            }
                          }
                        ]
                      }
                    ]
                  }
                }
              }
            }
          """
        )
      )
    }

  }

}
