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
import lucuma.odb.service.SmartGcalService
import skunk.Session

class sequence extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "Sequence Testing")

  override def initDb(s: Session[IO]): IO[Unit] = {
    val srv = SmartGcalService.fromSession(s)

    for {
      _ <- srv.insertGmosNorth(
        1,
        PosLong.unsafeFrom(1L),
        GmosNorthGrating.R831_G5302.some,
        GmosNorthFilter.RPrime.some,
        GmosNorthFpu.LongSlit_0_50.some,
        GmosXBinning.One,
        GmosYBinning.Two,
        BoundedInterval.openUpper(Wavelength.Min, Wavelength.Max),
        GmosGratingOrder.One.some,
        GmosAmpGain.Low,
        Gcal(
          Gcal.Lamp.fromContinuum(GcalContinuum.QuartzHalogen5W),
          GcalFilter.Gmos,
          GcalDiffuser.Ir,
          GcalShutter.Open
        ),
        PosInt.unsafeFrom(1),
        GcalBaselineType.Night,
        TimeSpan.unsafeFromMicroseconds(3_000_000L)
      )
    } yield ()
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
                       nextAtom {
                         time {
                           total {
                             microseconds
                           }
                         }
                         steps {
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
                    "nextAtom": {
                      "time": {
                        "total": {
                          "microseconds": 0
                        }
                      },
                      "steps": [
                        {
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
                       nextAtom {
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
                    "nextAtom": {
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
                         steps {
                           instrumentConfig {
                             gratingConfig {
                               wavelength { nanometers }
                             }
                           }
                         }
                       }
                       possibleFuture {
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

}
