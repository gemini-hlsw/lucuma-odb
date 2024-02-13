// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
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
import lucuma.core.enums.ObsActiveStatus.Inactive
import lucuma.core.enums.ObsStatus.Approved
import lucuma.core.enums.ObsStatus.New
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Wavelength
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.StepConfig.Gcal
import lucuma.core.util.TimeSpan
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.service.Services
import lucuma.odb.smartgcal.data.Gmos.GratingConfigKey
import lucuma.odb.smartgcal.data.Gmos.TableKey
import lucuma.odb.smartgcal.data.Gmos.TableRow
import lucuma.odb.smartgcal.data.SmartGcalValue
import lucuma.odb.smartgcal.data.SmartGcalValue.LegacyInstrumentConfig
import natchez.Trace.Implicits.noop
import skunk.Session

class programPlannedTime extends OdbSuite with ObservingModeSetupOperations {

  val pi: User   = TestUsers.Standard.pi(1, 30)
  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(pi, user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "Program Planned Time Testing")

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

    Enums.load(s).flatMap { e =>
      val services = Services.forUser(pi /* doesn't matter*/, e)(s)
      services.transactionally {
        services.smartGcalService.insertGmosNorth(1, tableRow)
      }
    }
  }

  // 1736.262500
  def createLongerGmosNorthLongSlitObservationAs(
    user: User,
    pid:  Program.Id,
    tids: Target.Id*
  ): IO[Observation.Id] =
    createObservationWithModeAs(
      user,
      pid,
      tids.toList,
      """
        gmosNorthLongSlit: {
          grating: R831_G5302,
          filter: R_PRIME,
          fpu: LONG_SLIT_0_50,
          explicitYBin: TWO,
          centralWavelength: {
            nanometers: 500
          }
          explicitSpatialOffsets: [
            {
              arcseconds: 10.0
            },
            {
              arcseconds: -10.0
            },
            {
              arcseconds: -10.0
            },
            {
              arcseconds: 10.0
            }
          ]
        }
      """
    )

  val ShortTime = BigDecimal("1729.200000")
  val LongTime  = BigDecimal("1736.262500")

  test("program level: single complete observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : $ShortTime
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : $ShortTime
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

  test("program level: single complete, but 'New' observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t), status = New)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : 0.000000
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : 0.000000
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

  test("program level: single complete, but 'Inactive' observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t), status = Approved, activeStatus = Inactive)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : 0.000000
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : 0.000000
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

  test("program level: two complete observations") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t))
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : ${ShortTime * 2}
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : ${ShortTime * 2}
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

  test("program level: one incomplete, one complete observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- createObservationWithNoModeAs(user, p, t)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                 "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : $ShortTime
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : $ShortTime
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

  private def moveObsToGroup(gid: Group.Id, oids: Observation.Id*): IO[Unit] =
    query(
      user = user,
      query = s"""
        mutation {
          updateObservations(
            input: {
              SET: {
                groupId: "$gid"
              }
              WHERE: {
                id: { IN: ${oids.asJson.spaces2} }
              }
            }
          ) {
            observations { id }
          }
        }
      """
    ).void

  test("program level: one group with one observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- moveObsToGroup(g, o)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                      "seconds" : $ShortTime
                    }
                  },
                  "maximum": {
                    "total" : {
                      "seconds" : $ShortTime
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

  test("group level..: one group with one observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- moveObsToGroup(g, o)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateRange {
                       minimum { total { seconds } }
                       maximum { total { seconds } }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateRange": {
                        "minimum": {
                          "total" : {
                            "seconds" : $ShortTime
                          }
                        },
                        "maximum": {
                          "total" : {
                            "seconds" : $ShortTime
                          }
                        }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

  test("program level: a group with observation and a top-level obs") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        g <- createGroupAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- moveObsToGroup(g, o)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : ${ShortTime * 2}
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : ${ShortTime * 2}
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

  test("program level: a simple OR group") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g, oShort, oLong)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                      "seconds" : $ShortTime
                    }
                  },
                  "maximum": {
                    "total" : {
                      "seconds" : $LongTime
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

  test("group level..: a simple OR group") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g, oShort, oLong)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateRange {
                       minimum { total { seconds } }
                       maximum { total { seconds } }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateRange": {
                        "minimum": {
                          "total" : {
                            "seconds" : $ShortTime
                          }
                        },
                        "maximum": {
                          "total" : {
                            "seconds" : $LongTime
                          }
                        }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )
    }
  }

  test("program level: a group with explicit minRequired, missing required children") {
    val setup: IO[Program.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        g  <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(2).some)
        o1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o2 <- createObservationWithNoModeAs(user, p, t)
        _  <- moveObsToGroup(g, o1, o2)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : 0.000000
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : 0.000000
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

  test("program level: two groups") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g0 <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong0  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g0, oShort0, oLong0)
        g1 <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong1  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g1, oShort1, oLong1)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   minimum { total { seconds } }
                   maximum { total { seconds } }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "minimum": {
                    "total" : {
                        "seconds" : ${ShortTime * 2}
                    }
                  },
                  "maximum": {
                    "total" : {
                        "seconds" : ${LongTime * 2}
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

  test("group level..: two groups") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g0 <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong0  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g0, oShort0, oLong0)
        g1 <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong1  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g1, oShort1, oLong1)
      } yield p

    setup.flatMap { pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateRange {
                       minimum { total { seconds } }
                       maximum { total { seconds } }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateRange": {
                        "minimum": {
                          "total" : {
                              "seconds" : $ShortTime
                          }
                        },
                        "maximum": {
                          "total" : {
                              "seconds" : $LongTime
                          }
                        }
                      }
                    }
                  },
                  {
                    "group": {
                      "timeEstimateRange": {
                        "minimum": {
                          "total" : {
                              "seconds" : $ShortTime
                          }
                        },
                        "maximum": {
                          "total" : {
                              "seconds" : $LongTime
                          }
                        }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )
    }
  }
}
