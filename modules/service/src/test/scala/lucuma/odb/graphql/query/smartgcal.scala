// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptySet
import cats.data.State
import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GcalArc
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
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

class smartgcal extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "SmartGcal Testing")

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e)(s)
      services.transactionally {

        val flat =
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

        val arc =
          SmartGcalValue(
            Gcal(
              Gcal.Lamp.fromArcs(NonEmptySet.one(GcalArc.CuArArc)),
              GcalFilter.None,
              GcalDiffuser.Visible,
              GcalShutter.Closed
            ),
            GcalBaselineType.Day,
            PosInt.unsafeFrom(1),
            LegacyInstrumentConfig(
              TimeSpan.unsafeFromMicroseconds(1_000_000L)
            )
          )

        def northRow(s: SmartGcalValue[LegacyInstrumentConfig]): TableRow.North =
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
            s
          )

        val tableRowArcN: TableRow.North =
          northRow(arc)

        val tableRowFlatN: TableRow.North =
          northRow(flat)

        def southRow(s: SmartGcalValue[LegacyInstrumentConfig]): TableRow.South =
          TableRow(
            PosLong.unsafeFrom(1),
            TableKey(
              GratingConfigKey(
                GmosSouthGrating.R600_G5324,
                GmosGratingOrder.One,
                BoundedInterval.unsafeOpenUpper(Wavelength.Min, Wavelength.Max)
              ).some,
              GmosSouthFilter.RPrime.some,
              GmosSouthFpu.LongSlit_0_50.some,
              GmosXBinning.One,
              GmosYBinning.Two,
              GmosAmpGain.Low
            ),
            s
          )

        val tableRowArcS: TableRow.South =
          southRow(arc)

        val tableRowFlatS: TableRow.South =
          southRow(flat)

        def defineN(
          id:         Int,
          row:        TableRow.North,
          stepOrder:  Int              = 1,
          disperser:  GmosNorthGrating = GmosNorthGrating.R831_G5302,
          low:        Int              = Wavelength.Min.pm.value.value,
          high:       Int              = Wavelength.Max.pm.value.value,
          expTimeSec: Int              = 1,
          count:      Int              = 1
        ): IO[Unit] =
          define(id, stepOrder, disperser, low, high, expTimeSec, count)(row, services.smartGcalService.insertGmosNorth)

        def defineS(
          id:         Int,
          row:        TableRow.South,
          stepOrder:  Int              = 1,
          disperser:  GmosSouthGrating = GmosSouthGrating.R600_G5324,
          low:        Int              = Wavelength.Min.pm.value.value,
          high:       Int              = Wavelength.Max.pm.value.value,
          expTimeSec: Int              = 1,
          count:      Int              = 1
        ): IO[Unit] =
          define(id, stepOrder, disperser, low, high, expTimeSec, count)(row, services.smartGcalService.insertGmosSouth)

        def define[G, L, U](
          id:         Int,
          stepOrder:  Int,
          disperser:  G,
          low:        Int,
          high:       Int,
          expTimeSec: Int,
          count:      Int
        )(
          tableRow:   TableRow[G, L, U],
          insert:     (Int, TableRow[G, L, U]) => IO[Unit]
        ): IO[Unit] = {

          import lucuma.core.optics.syntax.all.*

          val range = BoundedInterval.unsafeOpenUpper(
                        Wavelength.unsafeFromIntPicometers(low),
                        Wavelength.unsafeFromIntPicometers(high)
                      )

          val update: State[TableRow[G, L, U], Unit] =
            for {
              _ <- TableRow.line            := PosLong.unsafeFrom(stepOrder)
              _ <- TableRow.grating         := disperser
              _ <- TableRow.wavelengthRange := range
              _ <- TableRow.exposureTime    := TimeSpan.unsafeFromMicroseconds(expTimeSec * 1_000_000L)
              _ <- TableRow.stepCount       := PosInt.unsafeFrom(count)
            } yield ()

          insert(id, update.runS(tableRow).value)
        }

        for {
          // simple lookup
          _ <- defineN(1, tableRowFlatN, high = 500_000, expTimeSec = 1)
          _ <- defineN(2, tableRowFlatN, low  = 500_000, high = 600_000, expTimeSec = 2)
          _ <- defineN(3, tableRowFlatN, low  = 600_000, expTimeSec = 3)
          _ <- defineN(4, tableRowArcN,  expTimeSec = 1)

          _ <- defineS(1, tableRowFlatS, high = 500_000, expTimeSec = 1)
          _ <- defineS(2, tableRowFlatS, low  = 500_000, high = 600_000, expTimeSec = 2)
          _ <- defineS(3, tableRowFlatS, low  = 600_000, expTimeSec = 3)
          _ <- defineS(4, tableRowArcS,  expTimeSec = 1)

          // multi steps
          _ <- defineN(5, tableRowFlatN, stepOrder = 10, disperser = GmosNorthGrating.B600_G5303, expTimeSec = 4)
          _ <- defineN(6, tableRowFlatN, stepOrder =  9, disperser = GmosNorthGrating.B600_G5303, expTimeSec = 5)
          _ <- defineN(7, tableRowArcN, disperser = GmosNorthGrating.B600_G5303, expTimeSec = 1)

          // step count
          _ <- defineN(8, tableRowFlatN, disperser = GmosNorthGrating.B600_G5307, count = 2, expTimeSec = 6)
          _ <- defineN(9, tableRowArcN, disperser = GmosNorthGrating.B600_G5307, expTimeSec = 1)

        } yield ()
      }
    }
  }

  test("simple GN expansion") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    // Should pick definition 2, with the 2 second exposure time based on the
    // wavelength range [500, 600), which matches the 500nm observing wavelength

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
                                "instrumentConfig": {
                                  "exposure": {
                                    "seconds": 1.000000
                                  }
                                },
                                "stepConfig": {
                                  "stepType": "GCAL",
                                  "filter": "NONE"
                                }
                              }
                            ]
                          },
                          {
                            "steps": [
                              {
                                "instrumentConfig": {
                                  "exposure": {
                                    "seconds": 1.000000
                                  }
                                },
                                "stepConfig": {
                                  "stepType": "GCAL",
                                  "filter": "NONE"
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
              }
            }
          """
        )
      )
    }

  }

  test("simple GS expansion") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosSouthLongSlitObservationAs(user, p, List(t))
      } yield o

    // Should pick definition 2, with the 2 second exposure time based on the
    // wavelength range [500, 600), which matches the 500nm observing wavelength

    setup.flatMap { oid =>
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
             }
           """,
        expected = Right(
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosSouth": {
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

  test("multi steps") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: B600_G5303,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO
            }
          """
        )
      } yield o

    // Two definitions match the same key, distinguish and sort them by step
    // order.  (def 5 (5 seconds) followed by def 4 (4 seconds)).

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
              }
            }
          """
        )
      )
    }
  }

  test("step count") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createObservationWithModeAs(user, p, List(t),
          """
            gmosNorthLongSlit: {
              grating: B600_G5307,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 },
              explicitYBin: TWO
            }
          """
        )
      } yield o

    // One definition, with a step count of 2 (each 6 second)

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
              }
            }
          """
        )
      )
    }
  }

  test("missing definition") {
    val setup: IO[Observation.Id] =
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
             }
           """,
        expected = Left(List("Could not generate a sequence, missing Smart GCAL mapping: GmosNorth { grating: (R600_G5304, One, 500.000 nm), filter: RPrime, fpu: LongSlit_0_50, binning: 1x2, gain: Low }"))
      )
    }
  }
}
