// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime


class programPlannedTime extends ExecutionTestSupport {

  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  val user: User = serviceUser

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

  //              full setup + arcs            + flats            + sci w/ config chg  + science w/o config change
  val ShortTime = "960".sec + ("67.1".sec * 4) + ("57.1".sec * 4) + ("1266.1".sec * 4) + ("1251.1".sec * 3 * 2)

  // This observation will have 12 goals (lcm(3 dithers, 4 offsets)) but only
  // 10 datasets.  We spread out the 10 datasets as evenly as possible (1 per
  // goal for the first 10 goals).
  val LongTime  = "960".sec + ("67.1".sec * 10) + ("57.1".sec * 10) + ("1266.1".sec * 10)

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

  // test("program level: single complete, but 'New' observation") {
  //   val setup: IO[Program.Id] =
  //     for {
  //       p <- createProgram
  //       t <- createTargetWithProfileAs(user, p)
  //       _ <- createGmosNorthLongSlitObservationAs(user, p, List(t), status = New)
  //     } yield p

  //   setup.flatMap { pid =>
  //     expect(
  //       user  = user,
  //       query =
  //         s"""
  //            query {
  //              program(programId: "$pid") {
  //                timeEstimateRange {
  //                  minimum { total { seconds } }
  //                  maximum { total { seconds } }
  //                }
  //              }
  //            }
  //          """,
  //       expected = Right(
  //         json"""
  //           {
  //             "program": {
  //               "timeEstimateRange": {
  //                 "minimum": {
  //                   "total" : {
  //                       "seconds" : 0.000000
  //                   }
  //                 },
  //                 "maximum": {
  //                   "total" : {
  //                       "seconds" : 0.000000
  //                   }
  //                 }
  //               }
  //             }
  //           }
  //         """
  //       )
  //     )
  //   }

  // }

  test("program level: single complete, but 'Inactive' observation") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- setObservationWorkflowState(user, o, ObservationWorkflowState.Inactive)
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

  test("program level: two complete observations, but one is inactive") {
    val setup: IO[Program.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        _ <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        _ <- setObservationWorkflowState(user, o, ObservationWorkflowState.Inactive)
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
