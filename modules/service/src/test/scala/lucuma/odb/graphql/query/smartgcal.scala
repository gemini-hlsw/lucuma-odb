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
import lucuma.odb.smartgcal.data.GmosNorth.GratingConfigKey
import lucuma.odb.smartgcal.data.GmosNorth.TableKey
import lucuma.odb.smartgcal.data.GmosNorth.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import monocle.Lens
import monocle.Optional
import skunk.Session

class smartgcal extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "SmartGcal Testing")

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    val srv = SmartGcalService.fromSession(s)

    val tableRow: TableRow =
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

    def define(
      id:         Int,
      stepOrder:  Int              = 1,
      disperser:  GmosNorthGrating = GmosNorthGrating.R831_G5302,
      low:        Int              = Wavelength.Min.pm.value.value,
      high:       Int              = Wavelength.Max.pm.value.value,
      expTimeSec: Int              = 1,
      count:      Int              = 1
    ): IO[Unit] = {

      import lucuma.core.optics.syntax.all.*

      val range = BoundedInterval.unsafeOpenUpper(
                    Wavelength.unsafeFromIntPicometers(low),
                    Wavelength.unsafeFromIntPicometers(high)
                  )

      val update = for {
        _ <- TableRow.line            := PosLong.unsafeFrom(stepOrder)
        _ <- TableRow.grating         := disperser
        _ <- TableRow.wavelengthRange := range
        _ <- TableRow.exposureTime    := TimeSpan.unsafeFromMicroseconds(expTimeSec * 1_000_000L)
        _ <- TableRow.stepCount       := PosInt.unsafeFrom(count)
      } yield ()

      srv.insertGmosNorth(id, update.runS(tableRow).value)
    }

    for {
      // simple lookup
      _ <- define(1, high = 500_000, expTimeSec = 1)
      _ <- define(2, low  = 500_000, high = 600_000, expTimeSec = 2)
      _ <- define(3, low  = 600_000, expTimeSec = 3)

      // multi steps
      _ <- define(4, stepOrder = 10, disperser = GmosNorthGrating.B600_G5303, expTimeSec = 4)
      _ <- define(5, stepOrder =  9, disperser = GmosNorthGrating.B600_G5303, expTimeSec = 5)

      // step count
      _ <- define(6, disperser = GmosNorthGrating.B600_G5307, count = 2, expTimeSec = 6)

    } yield ()

  }

  test("simple lookup") {
    val setup: IO[(Program.Id, Observation.Id, Target.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, t)
      } yield (p, o, t)

    // Should pick definition 2, with the 2 second exposure time based on the
    // wavelength range [500, 600), which matches the 500nm observing wavelength

    setup.flatMap { case (pid, oid, _) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       nextAtom {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                           }
                           stepConfig {
                             stepType
                             ... on Gcal {
                               filter
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
                           stepConfig {
                             stepType
                             ... on Gcal {
                               filter
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
                  "science": {
                    "nextAtom": {
                      "steps": [
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 10.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "SCIENCE"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 2.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "GCAL",
                            "filter": "GMOS"
                          }
                        }
                      ]
                    },
                    "possibleFuture": [
                      {
                        "steps" : [
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 2.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "GCAL",
                              "filter" : "GMOS"
                            }
                          },
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 10.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "SCIENCE"
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 10.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "SCIENCE"
                            }
                          },
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 2.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "GCAL",
                              "filter" : "GMOS"
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 2.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "GCAL",
                              "filter" : "GMOS"
                            }
                          },
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 10.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "SCIENCE"
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 10.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "SCIENCE"
                            }
                          },
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 2.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "GCAL",
                              "filter" : "GMOS"
                            }
                          }
                        ]
                      },
                      {
                        "steps" : [
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 2.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "GCAL",
                              "filter" : "GMOS"
                            }
                          },
                          {
                            "instrumentConfig" : {
                              "exposure" : {
                                "seconds" : 10.000000
                              }
                            },
                            "stepConfig" : {
                              "stepType" : "SCIENCE"
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

  test("multi steps") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: B600_G5303,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 }
            }
          """
        )
      } yield (p, o)

    // Two definitions match the same key, distinguish and sort them by step
    // order.  (def 5 (5 seconds) followed by def 4 (4 seconds)).

    setup.flatMap { case (pid, oid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       nextAtom {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                           }
                           stepConfig {
                             stepType
                             ... on Gcal {
                               filter
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
                  "science": {
                    "nextAtom": {
                      "steps": [
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 10.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "SCIENCE"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 5.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "GCAL",
                            "filter": "GMOS"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 4.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "GCAL",
                            "filter": "GMOS"
                          }
                        }
                      ]
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

  test("step count") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: B600_G5307,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 }
            }
          """
        )
      } yield (p, o)

    // One definition, with a step count of 2 (each 6 second)

    setup.flatMap { case (pid, oid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       nextAtom {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                           }
                           stepConfig {
                             stepType
                             ... on Gcal {
                               filter
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
                  "science": {
                    "nextAtom": {
                      "steps": [
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 10.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "SCIENCE"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 6.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "GCAL",
                            "filter": "GMOS"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 6.000000
                            }
                          },
                          "stepConfig": {
                            "stepType": "GCAL",
                            "filter": "GMOS"
                          }
                        }
                      ]
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

  test("missing definition") {
    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: R600_G5304,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 }
            }
          """
        )
      } yield (p, o)

    setup.flatMap { case (pid, oid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     science {
                       nextAtom {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                           }
                           stepConfig {
                             stepType
                             ... on Gcal {
                               filter
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
        expected = Left(List("Could not generate a sequence, missing Smart GCAL mapping: GmosNorth { grating: (R600_G5304, One, 500.000 nm), filter: RPrime, fpu: LongSlit_0_50, binning: 1x2, gain: Low }"))
      )
    }
  }
}
