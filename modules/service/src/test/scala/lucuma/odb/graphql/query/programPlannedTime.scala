// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.graphql.input.AllocationInput


class programPlannedTime extends ExecutionTestSupportForGmos:

  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      10.minTimeSpan,
      PosInt.unsafeFrom(18)
    )

  val user: User = serviceUser

  // 1736.262500
  private def createLongerGmosNorthLongSlitObservationAs(
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
            { arcseconds: -10.0 },
            { arcseconds:  -5.0 },
            { arcseconds:   0.0 },
            { arcseconds:   5.0 },
            { arcseconds:  10.0 },
            { arcseconds:  15.0 }
          ]
        }
      """
    )

  val OneDataset =
    "600.0".sec + // exposure time
    "41.1".sec  + // readout
    "10.0".sec    // writeout

  val BaseTime =
    "960.0".sec +                                // full setup
    ("15.0".sec + "52.1".sec + "57.1".sec) * 3 + // (science fold + arc + flat)
    "15".sec * 3 +                               // calibration -> science, 3 wavelength blocks
    OneDataset * 18                              // 6 steps per 3 wavelength blocks

  val Offset_5arcsec  = "7.0".sec + "0.03125".sec

  val Offset_15arcsec = "7.0".sec + "0.09375".sec

  val Offset_30arcsec = "7.0".sec + "0.18750".sec

  // Only 2 offsets that matter in each of the 3 wavelength blocks.  The other
  // offset happens at the same time the science fold is moving.
  val ShortTime = BaseTime + (Offset_15arcsec + Offset_30arcsec) * 3

  // Only 5 offsets that matter in each of the 3 wavelength blocks.
  val LongTime  = BaseTime + Offset_5arcsec * 5 * 3

  private def createBandedGmosNorthLongSlitObservationAs(
    user: User,
    pid:  Program.Id,
    tid:  Target.Id,
    band: Option[ScienceBand]
  ): IO[Observation.Id] =
    for
      o <- createGmosNorthLongSlitObservationAs(user, pid, List(tid))
      _ <- setScienceBandAs(user, o, band)
    yield o

  val TenHours: TimeSpan = TimeSpan.fromHours(10.0).get
  val AllocationB1: AllocationInput = AllocationInput(TimeAccountingCategory.AR, ScienceBand.Band1, TenHours)
  val AllocationB2: AllocationInput = AllocationInput(TimeAccountingCategory.BR, ScienceBand.Band2, TenHours)
  val AllocationB3: AllocationInput = AllocationInput(TimeAccountingCategory.CA, ScienceBand.Band3, TenHours)

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

  test("program level range.: single complete observation"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : $ShortTime }
                    },
                    "maximum": {
                      "total": { "seconds" : $ShortTime }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level banded: single complete observation, no band"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": null,
                      "time": {
                        "total": { "seconds" : $ShortTime }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level banded: single complete observation, B2"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        _ <- setAllocationsAs(staff, p, List(AllocationB2))
        t <- createTargetWithProfileAs(user, p)
        o <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band2.some)
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": "BAND2",
                      "time": {
                        "total": { "seconds" : $ShortTime }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level range.: single complete, but 'Inactive' observation"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- setObservationWorkflowState(user, o, ObservationWorkflowState.Inactive)
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : 0.000000 }
                    },
                    "maximum": {
                      "total": { "seconds" : 0.000000 }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level range.: two complete observations"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : ${ShortTime * 2} }
                    },
                    "maximum": {
                      "total": { "seconds" : ${ShortTime * 2} }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level banded: two complete observations, same band"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        _  <- setAllocationsAs(user, p, List(AllocationB1))
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band1.some)
        o1 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band1.some)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": "BAND1",
                      "time": {
                        "total": { "seconds" : ${ShortTime * 2} }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level banded: two complete observations, different bands"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        _  <- setAllocationsAs(user, p, List(AllocationB1, AllocationB2))
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band1.some)
        o1 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band2.some)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": "BAND1",
                      "time": {
                        "total" : { "seconds" : $ShortTime }
                      }
                    }
                  },
                  {
                    "value": {
                      "band": "BAND2",
                      "time": {
                        "total" : { "seconds" : $ShortTime }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level banded: two complete observations, one no band"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        _  <- setAllocationsAs(user, p, List(AllocationB1))
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band1.some)
        o1 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, none)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": null,
                      "time": {
                        "total" : { "seconds" : $ShortTime }
                      }
                    }
                  },
                  {
                    "value": {
                      "band": "BAND1",
                      "time": {
                        "total" : { "seconds" : $ShortTime }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level range.: two complete observations, but one is inactive"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _  <- setObservationWorkflowState(user, o1, ObservationWorkflowState.Inactive)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : $ShortTime }
                    },
                    "maximum": {
                      "total": { "seconds" : $ShortTime }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level range.: one incomplete, one complete observation"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o1 <- createObservationWithNoModeAs(user, p, t)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                 "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : $ShortTime }
                    },
                    "maximum": {
                      "total": { "seconds" : $ShortTime }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level range.: one group with one observation"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- moveObsToGroup(g, o)
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : $ShortTime }
                    },
                    "maximum": {
                      "total": { "seconds" : $ShortTime }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("group level range...: one group with one observation"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- moveObsToGroup(g, o)
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateRange {
                       value {
                         minimum { total { seconds } }
                         maximum { total { seconds } }
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
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateRange": {
                        "value": {
                          "minimum": {
                            "total": { "seconds" : $ShortTime }
                          },
                          "maximum": {
                            "total": { "seconds" : $ShortTime }
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

  test("program level banded: one group with one observation"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        _ <- setAllocationsAs(user, p, List(AllocationB3))
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p)
        o <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band3.some)
        _ <- moveObsToGroup(g, o)
        _ <- runObscalcUpdate(p, o)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": "BAND3",
                      "time": {
                        "total" : { "seconds" : $ShortTime }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level range.: a group with observation and a top-level obs"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        g  <- createGroupAs(user, p)
        o1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _  <- moveObsToGroup(g, o1)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : ${ShortTime * 2} }
                    },
                    "maximum": {
                      "total": { "seconds" : ${ShortTime * 2} }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level banded: a group with observation and a top-level obs"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        _  <- setAllocationsAs(user, p, List(AllocationB1))
        t  <- createTargetWithProfileAs(user, p)
        o0 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band1.some)
        g  <- createGroupAs(user, p)
        o1 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band1.some)
        _  <- moveObsToGroup(g, o1)
        _  <- runObscalcUpdate(p, o0)
        _  <- runObscalcUpdate(p, o1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateBanded {
                   value {
                     band
                     time { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateBanded": [
                  {
                    "value": {
                      "band": "BAND1",
                      "time": {
                        "total": { "seconds" : ${ShortTime * 2} }
                      }
                    }
                  }
                ]
              }
            }
          """
        )
      )

  test("program level range.: a simple OR group"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g, oShort, oLong)
        _ <- runObscalcUpdate(p, oShort)
        _ <- runObscalcUpdate(p, oLong)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : $ShortTime }
                    },
                    "maximum": {
                      "total": { "seconds" : $LongTime }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("group level range...: a simple OR group"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        g <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        oLong  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g, oShort, oLong)
        _ <- runObscalcUpdate(p, oShort)
        _ <- runObscalcUpdate(p, oLong)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateRange {
                       value {
                         minimum { total { seconds } }
                         maximum { total { seconds } }
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
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateRange": {
                        "value": {
                          "minimum": {
                            "total": { "seconds" : $ShortTime }
                          },
                          "maximum": {
                            "total": { "seconds" : $LongTime }
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

  test("program level range.: a group with explicit minRequired, missing required children"):
    val setup: IO[Program.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        g  <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(2).some)
        o1 <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o2 <- createObservationWithNoModeAs(user, p, t)
        _  <- moveObsToGroup(g, o1, o2)
        _  <- runObscalcUpdate(p, o1)
        _  <- runObscalcUpdate(p, o2)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : 0.000000 }
                    },
                    "maximum": {
                      "total": { "seconds" : 0.000000 }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("program level range.: two groups"):
    val setup: IO[Program.Id] =
      for
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
        _ <- runObscalcUpdate(p, oShort0)
        _ <- runObscalcUpdate(p, oLong0)
        _ <- runObscalcUpdate(p, oShort1)
        _ <- runObscalcUpdate(p, oLong1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 timeEstimateRange {
                   value {
                     minimum { total { seconds } }
                     maximum { total { seconds } }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "program": {
                "timeEstimateRange": {
                  "value": {
                    "minimum": {
                      "total": { "seconds" : ${ShortTime * 2} }
                    },
                    "maximum": {
                      "total": { "seconds" : ${LongTime * 2} }
                    }
                  }
                }
              }
            }
          """
        )
      )

  test("group level range...: two groups"):
    val setup: IO[Program.Id] =
      for
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
        _ <- runObscalcUpdate(p, oShort0)
        _ <- runObscalcUpdate(p, oLong0)
        _ <- runObscalcUpdate(p, oShort1)
        _ <- runObscalcUpdate(p, oLong1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateRange {
                       value {
                         minimum { total { seconds } }
                         maximum { total { seconds } }
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
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateRange": {
                        "value": {
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
                  },
                  {
                    "group": {
                      "timeEstimateRange": {
                        "value": {
                          "minimum": {
                            "total": { "seconds" : $ShortTime }
                          },
                          "maximum": {
                            "total": { "seconds" : $LongTime }
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

  test("group level banded..: two groups"):
    val setup: IO[Program.Id] =
      for
        p <- createProgram
        _ <- setAllocationsAs(user, p, List(AllocationB1, AllocationB2))
        t <- createTargetWithProfileAs(user, p)
        g0 <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort0 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band2.some)
        oLong0  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g0, oShort0, oLong0)
        g1 <- createGroupAs(user, p, minRequired = NonNegShort.unsafeFrom(1).some)
        oShort1 <- createBandedGmosNorthLongSlitObservationAs(user, p, t, ScienceBand.Band2.some)
        oLong1  <- createLongerGmosNorthLongSlitObservationAs(user, p, t)
        _ <- moveObsToGroup(g1, oShort1, oLong1)
        _ <- runObscalcUpdate(p, oShort0)
        _ <- runObscalcUpdate(p, oLong0)
        _ <- runObscalcUpdate(p, oShort1)
        _ <- runObscalcUpdate(p, oLong1)
      yield p

    setup.flatMap: pid =>
      expect(
        user  = user,
        query =
          s"""
             query {
               program(programId: "$pid") {
                 groupElements {
                   group {
                     timeEstimateBanded {
                       value {
                         band
                         time { total { seconds } }
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
              "program": {
                "groupElements": [
                  {
                    "group": {
                      "timeEstimateBanded": [
                        {
                          "value": {
                            "band": null,
                            "time": { "total": { "seconds" : $LongTime } }
                          }
                        },
                        {
                          "value": {
                            "band": "BAND2",
                            "time": { "total": { "seconds" : $ShortTime } }
                          }
                        }
                      ]
                    }
                  },
                  {
                    "group": {
                      "timeEstimateBanded": [
                        {
                          "value": {
                            "band": null,
                            "time": { "total": { "seconds" : $LongTime } }
                          }
                        },
                        {
                          "value": {
                            "band": "BAND2",
                            "time": { "total": { "seconds" : $ShortTime } }
                          }
                        }
                      ]
                    }
                  }
                ]
              }
            }
          """
        )
      )