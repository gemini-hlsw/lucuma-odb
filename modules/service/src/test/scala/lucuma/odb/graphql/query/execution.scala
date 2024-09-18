// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.odb.data.Md5Hash
import munit.IgnoreSuite

// TODO: SEQUENCE UPDATE
@IgnoreSuite
class execution extends ExecutionTestSupport {

  // Additional cost of an arc (no sci fold move because we are already doing a flat)
  //  5.0 seconds for the Gcal configuration change (shutter, filter, diffuser)
  //  1.0 second for the arc exposure
  // 10.0 seconds for the write cost
  // 41.1 seconds readout (hamamatsu, 1x2, 12 amp, Low, Slow)
  // ----
  // 57.1 seconds per arc

  // 114.2 additional seconds for the two arcs produced by the sequence

  test("digest") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     setup {
                       full { seconds }
                       reacquisition { seconds }
                     }
                     acquisition {
                       observeClass
                       timeEstimate {
                         program { seconds }
                         partner { seconds }
                         nonCharged { seconds }
                         total { seconds }
                       }
                       offsets {
                         p { arcseconds }
                         q { arcseconds }
                       }
                       atomCount
                     }
                     science {
                       observeClass
                       timeEstimate {
                         program { seconds }
                         partner { seconds }
                         nonCharged { seconds }
                         total { seconds }
                       }
                       offsets {
                         p { arcseconds }
                         q { arcseconds }
                       }
                       atomCount
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "setup" : {
                      "full" : {
                        "seconds" : 960.000000
                      },
                      "reacquisition" : {
                        "seconds" : 300.000000
                      }
                    },
                    "acquisition" : {
                      "observeClass" : "ACQUISITION",
                      "timeEstimate" : {
                        "program" : {
                          "seconds" : 175.162500
                        },
                        "partner" : {
                          "seconds" : 0.000000
                        },
                        "nonCharged" : {
                          "seconds" : 0.000000
                        },
                        "total" : {
                          "seconds" : 175.162500
                        }
                      },
                      "offsets" : [
                        {
                          "p" : {
                            "arcseconds" : 0.000000
                          },
                          "q" : {
                            "arcseconds" : 0.000000
                          }
                        },
                        {
                          "p" : {
                            "arcseconds" : 10.000000
                          },
                          "q" : {
                            "arcseconds" : 0.000000
                          }
                        }
                      ],
                      "atomCount": 1
                    },
                    "science" : {
                      "observeClass" : "SCIENCE",
                      "timeEstimate" : {
                        "program" : {
                          "seconds" : 411.600000
                        },
                        "partner" : {
                          "seconds" : 471.800000
                        },
                        "nonCharged" : {
                          "seconds" : 0.000000
                        },
                        "total" : {
                          "seconds" : 883.400000
                        }
                      },
                      "offsets" : [
                        {
                          "p" : {
                            "arcseconds" : 0.000000
                          },
                          "q" : {
                            "arcseconds" : 0.000000
                          }
                        },
                        {
                          "p" : {
                            "arcseconds" : 0.000000
                          },
                          "q" : {
                            "arcseconds" : 15.000000
                          }
                        }
                      ],
                      "atomCount": 8
                    }
                  }
                }
              }
            }
          """
        )
      )
    }

  }

  test("digest: one bad") {

    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
      } yield (p, o)

    setup.flatMap { (pid, oid) =>
      expectIor(
        user  = pi,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 observations {
                   matches {
                     id
                     execution {
                       digest {
                         setup {
                           full { seconds }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"Could not generate a sequence from the observation $oid: observing mode"),
          json"""
            {
              "program": {
                "observations": {
                  "matches": [
                    {
                      "id": $oid,
                      "execution": {
                        "digest": null
                      }
                    }
                  ]
                }
              }
            }
          """
        )
      )
    }
  }

  test("digest: one good, one bad") {

    val setup: IO[(Program.Id, Observation.Id, Observation.Id)] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o1 <- createObservationWithNoModeAs(pi, p, t)
      } yield (p, o0, o1)

    setup.flatMap { (pid, oid0, oid1) =>
      expectIor(
        user  = pi,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 observations {
                   matches {
                     id
                     execution {
                       digest {
                         setup {
                           full { seconds }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"Could not generate a sequence from the observation $oid1: observing mode"),
          json"""
            {
              "program": {
                "observations": {
                  "matches": [
                    {
                      "id": $oid0,
                      "execution": {
                        "digest": {
                          "setup" : {
                            "full" : {
                              "seconds" : 960.000000
                            }
                          }
                        }
                      }
                    },
                    {
                      "id": $oid1,
                      "execution": {
                        "digest": null
                      }
                    }
                  ]
                }
              }
            }
          """
        )
      )
    }
  }

  test("digest: one bad, one good") {

    val setup: IO[(Program.Id, Observation.Id, Observation.Id)] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createObservationWithNoModeAs(pi, p, t)
        o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield (p, o0, o1)

    setup.flatMap { (pid, oid0, oid1) =>
      expectIor(
        user  = pi,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 observations {
                   matches {
                     id
                     execution {
                       digest {
                         setup {
                           full { seconds }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"Could not generate a sequence from the observation $oid0: observing mode"),
          json"""
            {
              "program": {
                "observations": {
                  "matches": [
                    {
                      "id": $oid0,
                      "execution": {
                        "digest": null
                      }
                    },
                    {
                      "id": $oid1,
                      "execution": {
                        "digest": {
                          "setup" : {
                            "full" : {
                              "seconds" : 960.000000
                            }
                          }
                        }
                      }
                    }
                  ]
                }
              }
            }
          """
        )
      )
    }
  }

  test("time estimate: config and detector estimates") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
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
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
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
                                      "seconds" : 44.200000
                                    },
                                    "exposure" : {
                                      "seconds" : 30.000000
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
                                    "seconds" : 44.200000
                                  }
                                },
                                "all" : [
                                  {
                                    "name" : "GMOS North",
                                    "description" : "GMOS North Hamamatsu Detector Array",
                                    "dataset" : {
                                      "estimate" : {
                                        "seconds" : 44.200000
                                      },
                                      "exposure" : {
                                        "seconds" : 30.000000
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
                                      "seconds" : 44.200000
                                    }
                                  }
                                ],
                                "estimate" : {
                                  "seconds" : 44.200000
                                }
                              },
                              "total" : {
                                "seconds" : 51.262500
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
              }
            }
          """
        )
      )
    }
  }

  test("time estimate: observation level") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             fragment categorizedTimeFields on CategorizedTime {
               program {
                 seconds
               }
               partner {
                 seconds
               }
               nonCharged {
                 seconds
               }
               total {
                 seconds
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
               observation(observationId: "$oid") {
                 execution {
                   digest {
                     setup {
                       full {
                         seconds
                       }
                       reacquisition {
                         seconds
                       }
                     }
                     science {
                       timeEstimate {
                         ...categorizedTimeFields
                       }
                     }
                   }
                   config {
                     gmosNorth {
                       science {
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "setup": {
                      "full": {
                        "seconds": 960.000000
                      },
                      "reacquisition": {
                        "seconds": 300.000000
                      }
                    },
                    "science": {
                      "timeEstimate" : {
                        "program" : {
                          "seconds" : 411.600000
                        },
                        "partner" : {
                          "seconds" : 471.800000
                        },
                        "nonCharged": {
                          "seconds": 0.000000
                        },
                        "total" : {
                          "seconds" : 883.400000
                        }
                      }
                    }
                  },
                  "config" : {
                    "gmosNorth": {
                      "science" : {
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
                                    "seconds" : 57.100000
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
                              }
                            ]
                          },
                          {
                            "steps" : [
                              {
                                "estimate" : {
                                  "total" : {
                                    "seconds" : 57.100000
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
              }
            }
          """
        )
      )
    }

  }

/* TODO: SEQUENCE UPDATE
  private val SetupZeroStepsGmosNorth: IO[Observation.Id] =
    for {
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    } yield o

  test("zero gmos north dynamic steps - GmosSequenceService") {
    SetupZeroStepsGmosNorth.flatMap { oid =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .gmosSequenceService
            .selectGmosNorthDynamicForObs(oid)(using xa)
            .compile
            .toList
        }
      }
    }.map(lst => assert(lst.isEmpty))
  }

  test("zero gmos north dynamic steps - SequenceService steps") {
    SetupZeroStepsGmosNorth.flatMap { oid =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(oid)(using xa)
        }
      }
    }.map(m => assert(m.isEmpty))
  }

  test("zero gmos north dynamic steps - SequenceService atom counts") {
    val m = SetupZeroStepsGmosNorth.flatMap { oid =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthCompletionState(oid)(using xa)
        }
      }
    }

    assertIO(m, Completion.Matcher.Empty)
  }

  private def setupOneStepGmosNorth(count: Int): IO[(Observation.Id, Step.Id, Dataset.Id)] = {
    import lucuma.odb.json.all.transport.given

    for {
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v)
      s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
      d <- recordDatasetAs(serviceUser, s, f"N18630101S$count%04d.fits")
      _ <- addDatasetEvent(d, DatasetStage.StartExpose)
      _ <- addDatasetEvent(d, DatasetStage.EndExpose)
      _ <- addDatasetEvent(d, DatasetStage.StartReadout)
      _ <- addDatasetEvent(d, DatasetStage.EndReadout)
      _ <- addDatasetEvent(d, DatasetStage.StartWrite)
      _ <- addEndStepEvent(s)
    } yield (o, s, d)
  }

  test("one gmos north dynamic step - GmosSequenceService") {
    setupOneStepGmosNorth(1).flatMap { case (o, s, _) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .gmosSequenceService
            .selectGmosNorthDynamicForObs(o)(using xa)
            .compile
            .toList
        }
      }.map(lst => assertEquals(lst, List(s -> GmosNorthScience0)))
    }
  }

  test("one gmos north dynamic step - SequenceService steps") {
    setupOneStepGmosNorth(2).flatMap { case (o, s, _) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(o)(using xa)
        }
      }.map { m =>
        assertEquals(m, Map(s -> (GmosNorthScience0, ScienceP00Q00)))
      }
    }
  }

  test("one gmos north dynamic step - SequenceService atom counts - incomplete dataset") {
    val m = setupOneStepGmosNorth(3).flatMap { case (o, _, _) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthCompletionState(o)(using xa)
        }
      }
    }
    assertIO(m.map(_.sci.combinedAtomMap), Completion.AtomMap.Empty)
  }

  test("one gmos north dynamic step - SequenceService atom counts - FAIL") {
    val m =
      for {
        osd <- setupOneStepGmosNorth(4)
        (o, s, d) = osd
        _   <- addDatasetEvent(d, DatasetStage.EndWrite)
        _   <- setQaState(d, DatasetQaState.Fail)
        m   <- withServices(pi) { services =>
          services.session.transaction.use { xa =>
            services
              .sequenceService
              .selectGmosNorthCompletionState(o)(using xa)
          }
        }
      } yield m

    assertIO(m.map(_.sci.combinedAtomMap), Completion.AtomMap.Empty)
  }

  test("one gmos north dynamic step - SequenceService atom counts - NULL QA") {
    val m =
      for {
        osd <- setupOneStepGmosNorth(5)
        (o, s, d) = osd
        _   <- addDatasetEvent(d, DatasetStage.EndWrite)
        m   <- withServices(pi) { services =>
          services.session.transaction.use { xa =>
            services
              .sequenceService
              .selectGmosNorthCompletionState(o)(using xa)
          }
        }
      } yield m

    assertIO(m.map(_.sci.combinedAtomMap), atomMap1((GmosNorthScience0, ScienceP00Q00)))
  }

  test("one gmos north dynamic step - SequenceService atom counts - Pass") {
    val m =
      for {
        osd <- setupOneStepGmosNorth(6)
        (o, s, d) = osd
        _   <- addDatasetEvent(d, DatasetStage.EndWrite)
        _   <- setQaState(d, DatasetQaState.Pass)
        m   <- withServices(pi) { services =>
          services.session.transaction.use { xa =>
            services
              .sequenceService
              .selectGmosNorthCompletionState(o)(using xa)
          }
        }
      } yield m

    assertIO(m.map(_.sci.combinedAtomMap), atomMap1((GmosNorthScience0, ScienceP00Q00)))
  }

  private val SetupTwoStepsGmosNorth: IO[(Observation.Id, Step.Id, Step.Id)] = {
    import lucuma.odb.json.all.transport.given

    for {
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 2)

      sSci  <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
      _     <- addEndStepEvent(sSci)

      sFlat <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0),    FlatStep)
      _     <- addEndStepEvent(sFlat)

    } yield (o, sSci, sFlat)
  }

  test("two gmos north dynamic steps - GmosSequenceService") {
    SetupTwoStepsGmosNorth.flatMap { case (o, sSci, sFlat) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .gmosSequenceService
            .selectGmosNorthDynamicForObs(o)(using xa)
            .compile
            .toList
            .map(_.toMap)
        }
      }.map { m =>
        assertEquals(m, Map(sSci -> GmosNorthScience0, sFlat -> GmosNorthFlat0))
      }
    }
  }

  test("two gmos north dynamic steps - SequenceService steps") {
    SetupTwoStepsGmosNorth.flatMap { case (o, sSci, sFlat) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(o)(using xa)
        }
      }.map { m =>
        assertEquals(m, Map(sSci -> (GmosNorthScience0, ScienceP00Q00), sFlat -> (GmosNorthFlat0, Flat)))
      }
    }
  }

  test("two gmos north dynamic steps - SequenceService atom counts") {
    val m = SetupTwoStepsGmosNorth.flatMap { case (o, _, _) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthCompletionState(o)(using xa)
        }
      }
    }

    assertIO(m.map(_.sci.combinedAtomMap), atomMap1((GmosNorthScience0, ScienceP00Q00), (GmosNorthFlat0, Flat)))
  }
*/
  test("clear execution digest") {

    val setup: IO[(Program.Id, Observation.Id, Step.Id)] = {
      import lucuma.odb.json.all.transport.given

      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v)
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
      } yield (p, o, s)
    }

    val isEmpty = setup.flatMap { case (p, o, s) =>
      withServices(pi) { services =>
        services.session.transaction.use { xa =>
          for {
            _ <- services.executionDigestService.insertOrUpdate(p, o, Md5Hash.Zero, ExecutionDigest.Zero)(using xa)
            _ <- services.executionEventService.insertStepEvent(s, StepStage.EndStep)(using xa, ().asInstanceOf) // shhh
            d <- services.executionDigestService.selectOne(p, o, Md5Hash.Zero)(using xa)
          } yield d.isEmpty
        }
      }
    }

    assertIOBoolean(isEmpty, "The execution digest should be removed")
  }

  test("execute the first step") {

    import lucuma.odb.json.all.transport.given

    // Atom.Id list before and after recording the steps for the first atom.
    val beforeAndAfter =
      for {
        p      <- createProgram
        t      <- createTargetWithProfileAs(pi, p)
        o      <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v      <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        before <- genGmosNorthSequence(o, SequenceType.Science, 7)
        a0     <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep)
        _      <- addEndStepEvent(s1)
        after  <- genGmosNorthSequence(o, SequenceType.Science, 6)
      } yield (before, after)

    // The tail of `before` should equal `after` since the first atom has been executed.
    val result = beforeAndAfter.map { case (before, after) => before.tail === after }

    assertIOBoolean(result)
  }

  test("execute the second step") {

    import lucuma.odb.json.all.transport.given

    // Atom.Id list before and after recording the steps for the first atom.
    val beforeAndAfter =
      for {
        p      <- createProgram
        t      <- createTargetWithProfileAs(pi, p)
        o      <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v      <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        before <- genGmosNorthSequence(o, SequenceType.Science, 7)
        a0     <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 1)
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthArc(0), ArcStep)
        _      <- addEndStepEvent(s0)
        after  <- genGmosNorthSequence(o, SequenceType.Science, 6)
      } yield (before, after)

    // `after` should be the same as `before` with atom at index 1 removed, since
    // the second atom was successfully executed.
    val result = beforeAndAfter.map { case (before, after) =>
      before.head :: before.tail.tail === after
    }

    assertIOBoolean(result)
  }

  test("incomplete atoms") {

    import lucuma.odb.json.all.transport.given

    // Atom.Id list before and after recording the steps for the first atom.
    val beforeAndAfter =
      for {
        p      <- createProgram
        t      <- createTargetWithProfileAs(pi, p)
        o      <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v      <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        before <- genGmosNorthSequence(o, SequenceType.Science, 5)
        a0     <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
        _      <- addEndStepEvent(s0)
        a1     <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 2)
        s1     <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthScience(5), scienceStep(0, 15))
        _      <- addEndStepEvent(s1)
        after  <- genGmosNorthSequence(o, SequenceType.Science, 5)
      } yield (before, after)

    // No atoms have been completed, just the first steps of two different atoms
    assertIOBoolean(beforeAndAfter.map { case (before, after) => before === after })
  }

  test("split atom") {

    import lucuma.odb.json.all.transport.given

    // Atom.Id list before and after recording the steps for the first atom.
    val beforeAndAfter =
      for {
        p      <- createProgram
        t      <- createTargetWithProfileAs(pi, p)
        o      <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v      <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        before <- genGmosNorthSequence(o, SequenceType.Science, 5)
        a0     <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(5), FlatStep)
        _      <- addEndStepEvent(s1)
        s2     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep)
        _      <- addEndStepEvent(s2)
        after  <- genGmosNorthSequence(o, SequenceType.Science, 5)
      } yield (before, after)

    // The first atom is broken by another step, so nothing has been successfully completed
    assertIOBoolean(beforeAndAfter.map { case (before, after) => before === after })
  }

  test("time accounting: observation level") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   timeCharge {
                     program { seconds }
                     partner { seconds }
                     nonCharged { seconds }
                     total { seconds }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "timeCharge": {
                    "program": {
                      "seconds": 0.000000
                    },
                    "partner": {
                      "seconds": 0.000000
                    },
                    "nonCharged": {
                      "seconds": 0.000000
                    },
                    "total": {
                      "seconds": 0.000000
                    }
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
