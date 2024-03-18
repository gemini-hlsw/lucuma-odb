// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.DatasetStage
import lucuma.core.enums.GcalBaselineType
import lucuma.core.enums.GcalContinuum
import lucuma.core.enums.GcalDiffuser
import lucuma.core.enums.GcalFilter
import lucuma.core.enums.GcalShutter
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosDtax
import lucuma.core.enums.GmosGratingOrder
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.Instrument
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepStage
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Offset
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gmos.GmosFpuMask
import lucuma.core.model.sequence.gmos.GmosGratingConfig
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Md5Hash
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.sequence.data.CompletedAtomMap
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

class execution extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "Sequence Testing")

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val tableRow1: TableRow.North =
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
    val tableRow2: TableRow.North =
      TableRow(
        PosLong.unsafeFrom(1),
        TableKey(
          GratingConfigKey(
            GmosNorthGrating.R831_G5302,
            GmosGratingOrder.One,
            BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
          ).some,
          GmosNorthFilter.RPrime.some,
          GmosNorthFpu.LongSlit_5_00.some,
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

    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e)(s)
      services.transactionally {
        services.smartGcalService.insertGmosNorth(1, tableRow1) *>
        services.smartGcalService.insertGmosNorth(2, tableRow2)
      }
    }
  }

  test("digest") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = user,
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
                          "seconds" : 357.600000
                        },
                        "nonCharged" : {
                          "seconds" : 0.000000
                        },
                        "total" : {
                          "seconds" : 769.200000
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
                      "atomCount": 6
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
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithNoModeAs(user, p, t)
      } yield (p, o)

    setup.flatMap { (pid, oid) =>
      expectIor(
        user  = user,
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
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o1 <- createObservationWithNoModeAs(user, p, t)
      } yield (p, o0, o1)

    setup.flatMap { (pid, oid0, oid1) =>
      expectIor(
        user  = user,
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
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createObservationWithNoModeAs(user, p, t)
        o1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield (p, o0, o1)

    setup.flatMap { (pid, oid0, oid1) =>
      expectIor(
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = user,
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
                                  "yBin": "TWO"
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
                                  "yBin": "ONE"
                                },
                                "dtax": "ZERO",
                                "roi": "CENTRAL_STAMP",
                                "gratingConfig": null,
                                "filter": "G_PRIME",
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
                                  "seconds": 30.000000
                                },
                                "readout": {
                                  "xBin": "ONE",
                                  "yBin": "ONE"
                                },
                                "dtax": "ZERO",
                                "roi": "CENTRAL_STAMP",
                                "gratingConfig": null,
                                "filter": "G_PRIME",
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
              }
            }
          """
        )
      )
    }

  }

  test("cannot generate, missing mode") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithNoModeAs(user, p, t)
      } yield o

    setup.flatMap { oid =>
      expectIor(
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = user,
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
                            "observeClass": "SCIENCE",
                            "steps" : [
                              {
                                "instrumentConfig" : {
                                  "gratingConfig" : {
                                    "grating" : "R831_G5302",
                                    "order" : "ONE",
                                    "wavelength" : {
                                      "nanometers" : 505.000
                                    }
                                  }
                                }
                              },
                              {
                                "instrumentConfig" : {
                                  "gratingConfig" : {
                                    "grating" : "R831_G5302",
                                    "order" : "ONE",
                                    "wavelength" : {
                                      "nanometers" : 505.000
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield (p, o)

    setup.flatMap { case (_, oid) =>
      expect(
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
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
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
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
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield (p, o)

    setup.flatMap { case (_, oid) =>
      expect(
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = user,
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
                          "seconds" : 357.600000
                        },
                        "nonCharged": {
                          "seconds": 0.000000
                        },
                        "total" : {
                          "seconds" : 769.200000
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
      t <- createTargetWithProfileAs(user, p)
      o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
    } yield o

  test("zero gmos north dynamic steps - GmosSequenceService") {
    SetupZeroStepsGmosNorth.flatMap { oid =>
      withServices(user) { services =>
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
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(oid)(using xa)
        }
      }
    }.map(m => assert(m.isEmpty))
  }

  test("zero gmos north dynamic steps - SequenceService atom counts") {
    SetupZeroStepsGmosNorth.flatMap { oid =>
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthCompletedAtomMap(oid)(using xa)
        }
      }
    }.map(m => assert(m.isEmpty))
  }

  private def addEndStepEvent(sid: Step.Id): IO[Unit] = {
    val q = s"""
      mutation {
        addStepEvent(input: {
          stepId: "$sid",
          stepStage: END_STEP
        }) {
          event {
            step {
              id
            }
          }
        }
      }
    """

    query(user, q).void
  }

  private def addDatasetEvent(did: Dataset.Id, stage: DatasetStage): IO[Unit] = {
    val q = s"""
      mutation {
        addDatasetEvent(input: {
          datasetId:    "$did",
          datasetStage: ${stage.tag.toUpperCase}
        }) {
          event {
            id
          }
        }
      }
    """

    query(user, q).void
  }

  private def setQaState(did: Dataset.Id, qa: DatasetQaState): IO[Unit] = {
    val q = s"""
        mutation {
          updateDatasets(input: {
            SET: {
              qaState: ${qa.tag.toUpperCase}
            },
            WHERE: {
              id: { EQ: "$did" }
            }
          }) {
            datasets {
              id
            }
          }
        }
    """

    query(user, q).void
  }

  private val OneSecond      = TimeSpan.unsafeFromMicroseconds( 1_000_000L)
  private val TenSeconds     = TimeSpan.unsafeFromMicroseconds(10_000_000L)

  private val GmosNorthScience0: GmosNorth =
    GmosNorth(
      TenSeconds,
      GmosCcdMode(GmosXBinning.One, GmosYBinning.Two, GmosAmpCount.Twelve, GmosAmpGain.Low, GmosAmpReadMode.Slow),
      GmosDtax.Zero,
      GmosRoi.FullFrame,
      GmosGratingConfig.North(GmosNorthGrating.R831_G5302, GmosGratingOrder.One, Wavelength.decimalNanometers.unsafeGet(BigDecimal("500.0"))).some,
      GmosNorthFilter.RPrime.some,
      GmosFpuMask.Builtin(GmosNorthFpu.LongSlit_0_50).some
    )

  private val GmosNorthScience5: GmosNorth =
    GmosNorthScience0.copy(
      gratingConfig = GmosNorthScience0.gratingConfig.map(_.copy(
        wavelength = Wavelength.decimalNanometers.unsafeGet(BigDecimal("505.0"))
      ))
    )

  private val GmosNorthFlat0 = GmosNorthScience0.copy(exposure = OneSecond)
  private val GmosNorthFlat5 = GmosNorthScience5.copy(exposure = OneSecond)

  private val Science00 = StepConfig.Science(Offset.Zero, StepGuideState.Enabled)
  private val Science15 = StepConfig.Science(Offset.microarcseconds.reverseGet((0, 15_000_000L)), StepGuideState.Enabled)
  private val Flat      = StepConfig.Gcal(Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W), GcalFilter.Gmos, GcalDiffuser.Ir, GcalShutter.Open)

  private def setupOneStepGmosNorth(count: Int): IO[(Observation.Id, Step.Id, Dataset.Id)] = {
    import lucuma.odb.json.all.transport.given

    for {
      p <- createProgram
      t <- createTargetWithProfileAs(user, p)
      o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      v <- recordVisitAs(user, Instrument.GmosNorth, o)
      a <- recordAtomAs(user, Instrument.GmosNorth, v)
      s <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthScience0, Science00)
      d <- recordDatasetAs(user, s, f"N18630101S$count%04d.fits")
      _ <- addDatasetEvent(d, DatasetStage.StartExpose)
      _ <- addDatasetEvent(d, DatasetStage.EndExpose)
      _ <- addDatasetEvent(d, DatasetStage.StartReadout)
      _ <- addDatasetEvent(d, DatasetStage.EndReadout)
      _ <- addDatasetEvent(d, DatasetStage.StartWrite)
      _ <- addEndStepEvent(s)
    } yield (o, s, d)
  }

  private def scienceSingleAtom[D](steps: CompletedAtomMap.StepMatch[D]*): CompletedAtomMap[D] =
    CompletedAtomMap.from(
      CompletedAtomMap.Key.science(steps*) -> PosInt.unsafeFrom(1)
    )

  test("one gmos north dynamic step - GmosSequenceService") {
    setupOneStepGmosNorth(1).flatMap { case (o, s, _) =>
      withServices(user) { services =>
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
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(o)(using xa)
        }
      }.map { m =>
        assertEquals(m, Map(s -> (GmosNorthScience0, Science00)))
      }
    }
  }

  test("one gmos north dynamic step - SequenceService atom counts - incomplete dataset") {
    val m = setupOneStepGmosNorth(3).flatMap { case (o, _, _) =>
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthCompletedAtomMap(o)(using xa)
        }
      }
    }

    assertIO(m, CompletedAtomMap.Empty)
  }

  test("one gmos north dynamic step - SequenceService atom counts - FAIL") {
    val m =
      for {
        osd <- setupOneStepGmosNorth(4)
        (o, s, d) = osd
        _   <- addDatasetEvent(d, DatasetStage.EndWrite)
        _   <- setQaState(d, DatasetQaState.Fail)
        m   <- withServices(user) { services =>
          services.session.transaction.use { xa =>
            services
              .sequenceService
              .selectGmosNorthCompletedAtomMap(o)(using xa)
          }
        }
      } yield m

    assertIO(m, CompletedAtomMap.Empty)
  }

  test("one gmos north dynamic step - SequenceService atom counts - NULL QA") {
    val m =
      for {
        osd <- setupOneStepGmosNorth(5)
        (o, s, d) = osd
        _   <- addDatasetEvent(d, DatasetStage.EndWrite)
        m   <- withServices(user) { services =>
          services.session.transaction.use { xa =>
            services
              .sequenceService
              .selectGmosNorthCompletedAtomMap(o)(using xa)
          }
        }
      } yield m

    assertIO(m, scienceSingleAtom((GmosNorthScience0, Science00)))
  }

  test("one gmos north dynamic step - SequenceService atom counts - Pass") {
    val m =
      for {
        osd <- setupOneStepGmosNorth(6)
        (o, s, d) = osd
        _   <- addDatasetEvent(d, DatasetStage.EndWrite)
        _   <- setQaState(d, DatasetQaState.Pass)
        m   <- withServices(user) { services =>
          services.session.transaction.use { xa =>
            services
              .sequenceService
              .selectGmosNorthCompletedAtomMap(o)(using xa)
          }
        }
      } yield m

    assertIO(m, scienceSingleAtom((GmosNorthScience0, Science00)))
  }

  private val SetupTwoStepsGmosNorth: IO[(Observation.Id, Step.Id, Step.Id)] = {
    import lucuma.odb.json.all.transport.given

    for {
      p <- createProgram
      t <- createTargetWithProfileAs(user, p)
      o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      v <- recordVisitAs(user, Instrument.GmosNorth, o)
      a <- recordAtomAs(user, Instrument.GmosNorth, v, stepCount = 2)

      sSci  <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthScience0, Science00)
      _     <- addEndStepEvent(sSci)

      sFlat <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthFlat0,    Flat)
      _     <- addEndStepEvent(sFlat)

    } yield (o, sSci, sFlat)
  }

  test("two gmos north dynamic steps - GmosSequenceService") {
    SetupTwoStepsGmosNorth.flatMap { case (o, sSci, sFlat) =>
      withServices(user) { services =>
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
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthSteps(o)(using xa)
        }
      }.map { m =>
        assertEquals(m, Map(sSci -> (GmosNorthScience0, Science00), sFlat -> (GmosNorthFlat0, Flat)))
      }
    }
  }

  test("two gmos north dynamic steps - SequenceService atom counts") {
    val m = SetupTwoStepsGmosNorth.flatMap { case (o, _, _) =>
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          services
            .sequenceService
            .selectGmosNorthCompletedAtomMap(o)(using xa)
        }
      }
    }

    assertIO(m, scienceSingleAtom((GmosNorthScience0, Science00), (GmosNorthFlat0, Flat)))
  }

  test("clear execution digest") {

    val setup: IO[(Program.Id, Observation.Id, Step.Id)] = {
      import lucuma.odb.json.all.transport.given

      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        v <- recordVisitAs(user, Instrument.GmosNorth, o)
        a <- recordAtomAs(user, Instrument.GmosNorth, v)
        s <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthScience0, Science00)
      } yield (p, o, s)
    }

    val isEmpty = setup.flatMap { case (p, o, s) =>
      withServices(user) { services =>
        services.session.transaction.use { xa =>
          for {
            _ <- services.executionDigestService.insertOrUpdate(p, o, Md5Hash.Zero, ExecutionDigest.Zero)(using xa)
            _ <- services.executionEventService.insertStepEvent(s, StepStage.EndStep)(using xa)
            d <- services.executionDigestService.selectOne(p, o, Md5Hash.Zero)(using xa)
          } yield d.isEmpty
        }
      }
    }

    assertIOBoolean(isEmpty, "The execution digest should be removed")
  }

  def genGmosNorthSequence(oid: Observation.Id, futureLimit: Int): IO[List[Atom.Id]] =
    query(
      user = user,
      query = s"""
        query {
          observation(observationId: "$oid") {
            execution {
              config(futureLimit: $futureLimit) {
                gmosNorth {
                  science {
                    nextAtom {
                      id
                    }
                    possibleFuture {
                      id
                    }
                  }
                }
              }
            }
          }
        }
      """
    ).map { json =>
      val sci = json.hcursor.downFields("observation", "execution", "config", "gmosNorth", "science")
      val n   = sci.downFields("nextAtom", "id").require[Atom.Id]
      val fs  = sci.downFields("possibleFuture").values.toList.flatMap(_.toList.map(_.hcursor.downField("id").require[Atom.Id]))
      n :: fs
    }

  test("execute the first step") {

    import lucuma.odb.json.all.transport.given

    // Atom.Id list before and after recording the steps for the first atom.
    val beforeAndAfter =
      for {
        p      <- createProgram
        t      <- createTargetWithProfileAs(user, p)
        o      <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        before <- genGmosNorthSequence(o, 5)
        v      <- recordVisitAs(user, Instrument.GmosNorth, o)
        a0     <- recordAtomAs(user, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthScience0, Science00)
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _      <- addEndStepEvent(s1)
        after  <- genGmosNorthSequence(o, 4)
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
        t      <- createTargetWithProfileAs(user, p)
        o      <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        before <- genGmosNorthSequence(o, 5)
        v      <- recordVisitAs(user, Instrument.GmosNorth, o)
        a0     <- recordAtomAs(user, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthFlat5, Flat)
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthScience5, Science15)
        _      <- addEndStepEvent(s1)
        after  <- genGmosNorthSequence(o, 4)
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
        t      <- createTargetWithProfileAs(user, p)
        o      <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        before <- genGmosNorthSequence(o, 5)
        v      <- recordVisitAs(user, Instrument.GmosNorth, o)
        a0     <- recordAtomAs(user, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthScience0, Science00)
        _      <- addEndStepEvent(s0)
        a1     <- recordAtomAs(user, Instrument.GmosNorth, v, stepCount = 2)
        s1     <- recordStepAs(user, a1, Instrument.GmosNorth, GmosNorthScience5, Science15)
        _      <- addEndStepEvent(s1)
        after  <- genGmosNorthSequence(o, 5)
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
        t      <- createTargetWithProfileAs(user, p)
        o      <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        before <- genGmosNorthSequence(o, 5)
        v      <- recordVisitAs(user, Instrument.GmosNorth, o)
        a0     <- recordAtomAs(user, Instrument.GmosNorth, v, stepCount = 2)
        s0     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthScience0, Science00)
        _      <- addEndStepEvent(s0)
        s1     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthFlat5, Flat)
        _      <- addEndStepEvent(s1)
        s2     <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthFlat0, Flat)
        _      <- addEndStepEvent(s2)
        after  <- genGmosNorthSequence(o, 5)
      } yield (before, after)

    // The first atom is broken by another step, so nothing has been successfully completed
    assertIOBoolean(beforeAndAfter.map { case (before, after) => before === after })
  }

  test("select min x-binning") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t0 <- createTargetWithGaussianAs(user, p, Angle.fromMicroarcseconds(647_200L))  // X-binning of 4
        t1 <- createTargetWithProfileAs(user, p)  // X-binning of 1
        o  <- createObservationWithModeAs(user, p, List(t0, t1),
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
        user  = user,
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
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = user,
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
