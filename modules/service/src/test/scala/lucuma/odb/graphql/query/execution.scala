// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObsActiveStatus
import lucuma.core.enums.ObsStatus
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.math.Angle
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.odb.data.Md5Hash
import lucuma.odb.sequence.data.Completion

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

  test("simple generation") {
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
                   config {
                     gmosNorth {
                       static {
                         stageMode
                         detector
                         mosPreImaging
                         nodAndShuffle {
                           posA { p { microarcseconds } }
                         }
                       }
                       acquisition {
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
                                 builtin
                                 customMask { slitWidth }
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "static": {
                        "stageMode": "FOLLOW_XY",
                        "detector": "HAMAMATSU",
                        "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
                        "nodAndShuffle": null
                      },
                      "acquisition": {
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
                                  "yBin": "TWO",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "FAST"
                                },
                                "dtax": "ZERO",
                                "roi": "CCD2",
                                "gratingConfig": null,
                                "filter": "G_PRIME",
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
                                  "yBin": "ONE",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "FAST"
                                },
                                "dtax": "ZERO",
                                "roi": "CENTRAL_STAMP",
                                "gratingConfig": null,
                                "filter": "G_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
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
                                  "seconds": 30.000000
                                },
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "ONE",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "FAST"
                                },
                                "dtax": "ZERO",
                                "roi": "CENTRAL_STAMP",
                                "gratingConfig": null,
                                "filter": "G_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
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
              }
            }
          """
        )
      )
    }

  }

  test("ITC failure") {
    // Creates an observation with a central wavelength of 666, which prompts
    // the test ITC to produce an error instead of a result. (See OdbSuite
    // itcClient.)
    def createObservation(
      user: User,
      pid:  Program.Id,
      tids: List[Target.Id]
    ): IO[Observation.Id] =
      createObservationWithModeAs(
        user,
        pid,
        tids,
        """
          gmosNorthLongSlit: {
            grating: R831_G5302,
            fpu: LONG_SLIT_0_50,
            centralWavelength: { nanometers: 666 }
          }
        """,
        ObsStatus.Approved,
        ObsActiveStatus.Active
      )

    val setup: IO[(Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservation(pi, p, List(t))
      } yield (o, t)

    setup.flatMap { case (oid, tid) =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps { instrumentConfig { exposure { seconds } } }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
          expected = List(s"ITC returned errors: Target '$tid': Artifical exception for test cases.").asLeft
      )
    }

  }

  test("cannot generate, missing mode") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
      } yield o

    setup.flatMap { oid =>
      expectIor(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       static {
                         stageMode
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
              "observation": {
                "execution": {
                  "config": null
                }
              }
            }
          """
        )
      )
    }
  }

  test("simple generation - limited future") {
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
                   config(futureLimit: 1) {
                     gmosNorth {
                       science {
                         nextAtom {
                           observeClass
                         }
                         possibleFuture {
                           observeClass
                           steps {
                             instrumentConfig {
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
                         hasMore
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "observeClass": "SCIENCE"
                        },
                        "possibleFuture": [
                          {
                            "observeClass": "PARTNER_CAL",
                            "steps" : [
                              {
                                "instrumentConfig" : {
                                  "gratingConfig" : {
                                    "grating" : "R831_G5302",
                                    "order" : "ONE",
                                    "wavelength" : {
                                      "nanometers" : 500.000
                                    }
                                  }
                                }
                              }
                            ]
                          }
                        ],
                        "hasMore": true
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }

  }

  test("simple generation - too many future atoms") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield (p, o)

    setup.flatMap { case (_, oid) =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config(futureLimit: 101) {
                     gmosNorth {
                       science {
                         possibleFuture {
                           observeClass
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = List("Argument 'futureLimit' is invalid: Future limit must range from 0 to 100, but was 101.").asLeft
      )
    }
  }

  test("explicit offsets") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitSpatialOffsets: [
                { arcseconds: -15.0 },
                { arcseconds:  15.0 },
                { arcseconds:  15.0 },
                { arcseconds: -15.0 }
              ]
            }
          """
        )
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
                     science {
                       offsets {
                         q { arcseconds }
                       }
                     }
                   }
                   config {
                     gmosNorth {
                       science {
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "science": {
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
                    }
                  },
                  "config": {
                    "gmosNorth": {
                      "science": {
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
                            "description" : "Arc: q -15.0″, λ 500.0 nm",
                            "steps" : [
                              {
                                "stepConfig" : {
                                  "stepType" : "GCAL"
                                }
                              }
                            ]
                          },
                          {
                            "description" : "Arc: q 15.0″, λ 505.0 nm",
                            "steps" : [
                              {
                                "stepConfig" : {
                                  "stepType" : "GCAL"
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
              }
            }
          """
        )
      )
    }

  }

  test("explicit wavelength dithers") {

    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithModeAs(pi, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R831_G5302,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO,
              explicitWavelengthDithers: [
                { nanometers: -5.0 },
                { nanometers:  0.0 },
                { nanometers:  5.0 }
              ]
            }
          """
        )
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
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
                            "description" : "Arc: q 0.0″, λ 495.0 nm",
                            "steps" : [
                              {
                                "instrumentConfig" : {
                                  "gratingConfig" : {
                                    "wavelength" : {
                                      "nanometers" : 495.000
                                    }
                                  }
                                }
                              }
                            ]
                          },
                          {
                            "description" : "Arc: q 15.0″, λ 500.0 nm",
                            "steps" : [
                              {
                                "instrumentConfig" : {
                                  "gratingConfig" : {
                                    "wavelength" : {
                                      "nanometers" : 500.000
                                    }
                                  }
                                }
                              }
                            ]
                          },
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
                            "description" : "Arc: q 15.0″, λ 505.0 nm",
                            "steps" : [
                              {
                                "instrumentConfig" : {
                                  "gratingConfig" : {
                                    "wavelength" : {
                                      "nanometers" : 505.000
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
              }
            }
          """
        )
      )
    }

  }

  test("user cannot access program") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi2,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     instrument
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "observation": null
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
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield (p, o)

    setup.flatMap { case (_, oid) =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosSouth {
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosSouth": null
                  }
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
      s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
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

      sSci  <- recordStepAs(serviceUser, a, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
      _     <- addEndStepEvent(sSci)

      sFlat <- recordStepAs(serviceUser, a, Instrument.GmosNorth, GmosNorthFlat0,    Flat)
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

  test("clear execution digest") {

    val setup: IO[(Program.Id, Observation.Id, Step.Id)] = {
      import lucuma.odb.json.all.transport.given

      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v)
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
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
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
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
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthArc0, Arc)
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
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _      <- addEndStepEvent(s0)
        a1     <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, stepCount = 2)
        s1     <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, GmosNorthScience5, ScienceP00Q15)
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
        s0     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthScience0, ScienceP00Q00)
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat5, Flat)
        _      <- addEndStepEvent(s1)
        s2     <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _      <- addEndStepEvent(s2)
        after  <- genGmosNorthSequence(o, SequenceType.Science, 5)
      } yield (before, after)

    // The first atom is broken by another step, so nothing has been successfully completed
    assertIOBoolean(beforeAndAfter.map { case (before, after) => before === after })
  }

  test("select min x-binning") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t0 <- createTargetWithGaussianAs(pi, p, Angle.fromMicroarcseconds(647_200L))  // X-binning of 4
        t1 <- createTargetWithProfileAs(pi, p)  // X-binning of 1
        o  <- createObservationWithModeAs(pi, p, List(t0, t1),
               // use a 5" slit so that won't be a factor
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_5_00,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitYBin: TWO
                 }
               """
             )
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps {
                             instrumentConfig {
                               readout {
                                 xBin
                                 yBin
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
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "steps": [
                            {
                              "instrumentConfig": {
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "TWO"
                                }
                              }
                            },
                            {
                              "instrumentConfig": {
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "TWO"
                                }
                              }
                            }
                          ]
                        }
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

  /*
        withServices(pi) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(oid)(using xa)
        }
      }
   */

  test("unimplemented calibration role") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.Photometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           observeClass
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = List("GMOS North photometric observation sequence generation not supported.").asLeft
      )
    }
  }

  test("spec phot") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.SpectroPhotometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
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
                                 builtin
                                 customMask { slitWidth }
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "observeClass": "SCIENCE",
                          "steps": [
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 10.000000
                                },
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "TWO",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "SLOW"
                                },
                                "dtax": "ZERO",
                                "roi": "FULL_FRAME",
                                "gratingConfig": {
                                  "grating": "R831_G5302",
                                  "order": "ONE",
                                  "wavelength": {
                                    "nanometers": 500.000
                                  }
                                },
                                "filter": "R_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
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
                            },
                            {
                              "observeClass": "PARTNER_CAL",
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 1.000000
                                },
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "TWO",
                                  "ampCount": "TWELVE",
                                  "ampGain": "LOW",
                                  "ampReadMode": "SLOW"
                                },
                                "dtax": "ZERO",
                                "roi": "FULL_FRAME",
                                "gratingConfig": {
                                  "grating" : "R831_G5302",
                                  "order" : "ONE",
                                  "wavelength" : {
                                    "nanometers" : 500.000
                                  }
                                },
                                "filter": "R_PRIME",
                                "fpu": {
                                  "builtin": "LONG_SLIT_0_50",
                                  "customMask": null
                                }
                              },
                              "stepConfig": {
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

  test("spec phot, small custom wavelength dither") {
    // simultaneous coverage 235 nm, so dither up to 23.5 nm is ignored
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o <- createObservationWithModeAs(pi, p, List(t),
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitWavelengthDithers: [
                     {
                       nanometers:  0.0
                     },
                     {
                       nanometers: 10.0
                     },
                     {
                       nanometers: 23.5
                     }
                   ],
                   explicitYBin: TWO
                 }
               """
             )
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.SpectroPhotometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
                                 }
                               }
                             }
                           }
                         }
                         possibleFuture {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "steps": [
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength": {
                                    "nanometers": 500.000
                                  }
                                }
                              }
                            },
                            {
                              "observeClass": "PARTNER_CAL",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength" : {
                                    "nanometers" : 500.000
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
              }
            }
          """
        )
      )
    }
  }

  test("spec phot, large custom wavelength dither") {
    // simultaneous coverage 235 nm, so dither over 23.5 nm is tracked
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o <- createObservationWithModeAs(pi, p, List(t),
               """
                 gmosNorthLongSlit: {
                   grating: R831_G5302,
                   filter: R_PRIME,
                   fpu: LONG_SLIT_0_50,
                   centralWavelength: {
                     nanometers: 500
                   },
                   explicitWavelengthDithers: [
                     {
                       nanometers:  0.0
                     },
                     {
                       nanometers: 23.501
                     }
                   ],
                   explicitYBin: TWO
                 }
               """
             )
        _ <- withServices(serviceUser) { services =>
               services.session.transaction.use { xa =>
                 services.calibrationsService.setCalibrationRole(o, CalibrationRole.SpectroPhotometric.some)(using xa)
               }
             }
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 execution {
                   config {
                     gmosNorth {
                       science {
                         nextAtom {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
                                 }
                               }
                             }
                           }
                         }
                         possibleFuture {
                           steps {
                             observeClass
                             instrumentConfig {
                               gratingConfig {
                                 wavelength {
                                   nanometers
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "science": {
                        "nextAtom": {
                          "steps": [
                            {
                              "observeClass": "SCIENCE",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength": {
                                    "nanometers": 500.000
                                  }
                                }
                              }
                            },
                            {
                              "observeClass": "PARTNER_CAL",
                              "instrumentConfig": {
                                "gratingConfig": {
                                  "wavelength" : {
                                    "nanometers" : 500.000
                                  }
                                }
                              }
                            }
                          ]
                        },
                        "possibleFuture": [
                          {
                            "steps": [
                              {
                                "observeClass": "PARTNER_CAL",
                                "instrumentConfig": {
                                  "gratingConfig": {
                                    "wavelength" : {
                                      "nanometers" : 523.501
                                    }
                                  }
                                }
                              },
                              {
                                "observeClass": "SCIENCE",
                                "instrumentConfig": {
                                  "gratingConfig": {
                                    "wavelength": {
                                      "nanometers": 523.501
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
              }
            }
          """
        )
      )
    }
  }
}
