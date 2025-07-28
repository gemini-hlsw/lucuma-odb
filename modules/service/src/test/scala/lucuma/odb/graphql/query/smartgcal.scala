// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptySet
import cats.data.State
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
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
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import lucuma.odb.service.SmartGcalService
import lucuma.odb.smartgcal.data.Flamingos2
import lucuma.odb.smartgcal.data.Gmos
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
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

  // 3 exposures, one per adjustment (0, 0), (5, +15), (-5, -15)
  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      10.secTimeSpan,
      PosInt.unsafeFrom(3)
    )

  override def dbInitialization: Option[Session[IO] => IO[Unit]] = Some { s =>
    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e, None)(s)
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

        def northRow(s: SmartGcalValue[LegacyInstrumentConfig]): Gmos.TableRow.North =
          Gmos.TableRow(
            PosLong.unsafeFrom(1),
            Gmos.TableKey(
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

        val tableRowArcGmosN: Gmos.TableRow.North =
          northRow(arc)

        val tableRowFlatGmosN: Gmos.TableRow.North =
          northRow(flat)

        def southRow(s: SmartGcalValue[LegacyInstrumentConfig]): Gmos.TableRow.South =
          Gmos.TableRow(
            PosLong.unsafeFrom(1),
            Gmos.TableKey(
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

        val tableRowArcGmosS: Gmos.TableRow.South =
          southRow(arc)

        val tableRowFlatGmosS: Gmos.TableRow.South =
          southRow(flat)

        def f2Row(s: SmartGcalValue[LegacyInstrumentConfig]): Flamingos2.TableRow =
          Flamingos2.TableRow(
            PosLong.unsafeFrom(1),
            Flamingos2.TableKey(
              Flamingos2Disperser.R3000.some,
              Flamingos2Filter.JH,
              Flamingos2Fpu.LongSlit8.some
            ),
            s
          )
        val tableRowArcF2: Flamingos2.TableRow =
          f2Row(arc)

        val tableRowFlatF2: Flamingos2.TableRow =
          f2Row(flat)

        def defineGmosN(
          id:         Int,
          row:        Gmos.TableRow.North,
          stepOrder:  Int              = 1,
          disperser:  GmosNorthGrating = GmosNorthGrating.R831_G5302,
          low:        Int              = Wavelength.Min.pm.value.value,
          high:       Int              = Wavelength.Max.pm.value.value,
          expTimeSec: Int,
          count:      Int              = 1
        ): IO[Unit] =
          Services.asSuperUser:
            defineGmos(id, stepOrder, disperser, low, high, expTimeSec, count)(row, services.smartGcalService.insertGmosNorth)

        def defineGmosS(
          id:         Int,
          row:        Gmos.TableRow.South,
          stepOrder:  Int              = 1,
          disperser:  GmosSouthGrating = GmosSouthGrating.R600_G5324,
          low:        Int              = Wavelength.Min.pm.value.value,
          high:       Int              = Wavelength.Max.pm.value.value,
          expTimeSec: Int,
          count:      Int              = 1
        ): IO[Unit] =
          Services.asSuperUser:
            defineGmos(id, stepOrder, disperser, low, high, expTimeSec, count)(row, services.smartGcalService.insertGmosSouth)

        def defineGmos[G, L, U](
          id:         Int,
          stepOrder:  Int,
          disperser:  G,
          low:        Int,
          high:       Int,
          expTimeSec: Int,
          count:      Int
        )(
          tableRow:   Gmos.TableRow[G, L, U],
          insert:     (Int, Gmos.TableRow[G, L, U]) => IO[Unit]
        ): IO[Unit] = {

          import lucuma.core.optics.syntax.all.*

          val range = BoundedInterval.unsafeOpenUpper(
                        Wavelength.unsafeFromIntPicometers(low),
                        Wavelength.unsafeFromIntPicometers(high)
                      )

          val update: State[Gmos.TableRow[G, L, U], Unit] =
            for {
              _ <- Gmos.TableRow.line            := PosLong.unsafeFrom(stepOrder)
              _ <- Gmos.TableRow.grating         := disperser
              _ <- Gmos.TableRow.wavelengthRange := range
              _ <- Gmos.TableRow.exposureTime    := TimeSpan.unsafeFromMicroseconds(expTimeSec * 1_000_000L)
              _ <- Gmos.TableRow.stepCount       := PosInt.unsafeFrom(count)
            } yield ()

          insert(id, update.runS(tableRow).value)
        }

        def defineF2(
          id:         Int,
          stepOrder:  Int,
          expTimeSec: Int,
          count:      Int,
          tableRow:   Flamingos2.TableRow
        ): IO[Unit] = {
          import lucuma.core.optics.syntax.all.*

          val update: State[Flamingos2.TableRow, Unit] =
            for {
              _ <- Flamingos2.TableRow.line            := PosLong.unsafeFrom(stepOrder)
              _ <- Flamingos2.TableRow.disperser       := tableRow.key.disperser
              _ <- Flamingos2.TableRow.exposureTime    := TimeSpan.unsafeFromMicroseconds(expTimeSec * 1_000_000L)
              _ <- Flamingos2.TableRow.stepCount       := PosInt.unsafeFrom(count)
            } yield ()

          Services.asSuperUser:
            services.smartGcalService.insertFlamingos2(id, update.runS(tableRow).value)
        }

        for {
          // simple lookup
          _ <- defineGmosN(1, tableRowFlatGmosN, high = 500_000, expTimeSec = 1)
          _ <- defineGmosN(2, tableRowFlatGmosN, low  = 500_000, high = 600_000, expTimeSec = 2)
          _ <- defineGmosN(3, tableRowFlatGmosN, low  = 600_000, expTimeSec = 3)
          _ <- defineGmosN(4, tableRowArcGmosN,  expTimeSec = 1)

          _ <- defineGmosS(1, tableRowFlatGmosS, high = 500_000, expTimeSec = 1)
          _ <- defineGmosS(2, tableRowFlatGmosS, low  = 500_000, high = 600_000, expTimeSec = 2)
          _ <- defineGmosS(3, tableRowFlatGmosS, low  = 600_000, expTimeSec = 3)
          _ <- defineGmosS(4, tableRowArcGmosS,  expTimeSec = 1)

          _ <- defineF2(1, 1, expTimeSec = 1, count = 1, tableRow = tableRowFlatF2)
          _ <- defineF2(2, 1, expTimeSec = 1, count = 1, tableRow = tableRowArcF2)

          // multi steps
          _ <- defineGmosN(5, tableRowFlatGmosN, stepOrder = 10, disperser = GmosNorthGrating.R600_G5304, expTimeSec = 4)
          _ <- defineGmosN(6, tableRowFlatGmosN, stepOrder =  9, disperser = GmosNorthGrating.R600_G5304, expTimeSec = 5)
          _ <- defineGmosN(7, tableRowArcGmosN, disperser = GmosNorthGrating.R600_G5304, expTimeSec = 1)

          // step count
          _ <- defineGmosN(8, tableRowFlatGmosN, disperser = GmosNorthGrating.B480_G5309, count = 2, expTimeSec = 6)
          _ <- defineGmosN(9, tableRowArcGmosN, disperser = GmosNorthGrating.B480_G5309, expTimeSec = 1)

        } yield ()
      }
    }
  }

  val AtomQuery: String =
    s"""
      steps {
      instrumentConfig {
        exposure { seconds }
      }
      stepConfig {
        stepType
        ... on Gcal {
           continuum
           arcs
        }
      }
    }
  """

  test("simple GN expansion") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield o

    // For the first two atoms, at +0nm and +5nm, it should pick definition 2,
    // with the 2 second exposure time based on the wavelength range [500, 600),
    // which matches the 500nm observing wavelength.  The last atom has a -5nm
    // dither which drops it into the [0, 500) range with a 1 second exposure
    // time.

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
                           $AtomQuery
                         }
                         possibleFuture {
                           $AtomQuery
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
                                  "seconds": 1.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "GCAL",
                                "continuum": null,
                                "arcs": [ "CU_AR_ARC" ]
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
                                "continuum": "QUARTZ_HALOGEN5",
                                "arcs": []
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 10.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "SCIENCE"
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
                                  "continuum": null,
                                  "arcs": [ "CU_AR_ARC" ]
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
                                  "continuum": "QUARTZ_HALOGEN5",
                                  "arcs": []
                                }
                              },
                              {
                                "instrumentConfig": {
                                  "exposure": {
                                    "seconds": 10.000000
                                  }
                                },
                                "stepConfig": {
                                  "stepType": "SCIENCE"
                                }
                              }
                            ]
                          },
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
                                  "continuum": null,
                                  "arcs": [ "CU_AR_ARC" ]
                                }
                              },
                              {
                                "instrumentConfig": {
                                  "exposure": {
                                    "seconds": 1.000000
                                  }
                                },
                                "stepConfig": {
                                  "stepType": "GCAL",
                                  "continuum": "QUARTZ_HALOGEN5",
                                  "arcs": []
                                }
                              },
                              {
                                "instrumentConfig": {
                                  "exposure": {
                                    "seconds": 10.000000
                                  }
                                },
                                "stepConfig": {
                                  "stepType": "SCIENCE"
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
                           $AtomQuery
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
                                  "seconds": 1.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "GCAL",
                                "continuum": null,
                                "arcs": [ "CU_AR_ARC" ]
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
                                "continuum": "QUARTZ_HALOGEN5",
                                "arcs": []
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 10.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "SCIENCE"
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
              grating: R600_G5304,
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
                           $AtomQuery
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
                                  "seconds": 1.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "GCAL",
                                "continuum": null,
                                "arcs": [ "CU_AR_ARC" ]
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
                                "continuum": "QUARTZ_HALOGEN5",
                                "arcs": []
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
                                "continuum": "QUARTZ_HALOGEN5",
                                "arcs": []
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 10.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "SCIENCE"
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
              grating: B480_G5309,
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
                           $AtomQuery
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
                                  "seconds": 1.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "GCAL",
                                "continuum": null,
                                "arcs": [ "CU_AR_ARC" ]
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
                                "continuum": "QUARTZ_HALOGEN5",
                                "arcs": []
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
                                "continuum": "QUARTZ_HALOGEN5",
                                "arcs": []
                              }
                            },
                            {
                              "instrumentConfig": {
                                "exposure": {
                                  "seconds": 10.000000
                                }
                              },
                              "stepConfig": {
                                "stepType": "SCIENCE"
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
              grating: B1200_G5301,
              filter:  R_PRIME,
              fpu:     LONG_SLIT_0_50,
              centralWavelength: { nanometers: 500 }
              explicitYBin: TWO
            }
          """
        )
      } yield o

    setup.flatMap: oid =>
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
                           $AtomQuery
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = List(
          s"Could not generate a sequence for $oid: missing Smart GCAL mapping: GmosNorth { grating: (B1200_G5301, One, 500.000 nm), filter: RPrime, fpu: LongSlit_0_50, binning: 1x2, gain: Low }"
        ).asLeft
      )
  }

}
