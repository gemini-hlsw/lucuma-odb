// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.data.Md5Hash
import lucuma.odb.graphql.input.AddStepEventInput
import lucuma.odb.service.Services

class executionDigest extends ExecutionTestSupportForGmos {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10),
    )

  // * Arc:
  // (there is an offset cost but it is subsumed by the science fold movement)
  //   * Science Fold.: 15.0
  //   * Exposure Time:  1.0
  //   * Readout......: 41.1
  //   * Writeout.....: 10.0
  //                    ----
  //                    67.1

  // * Flat:
  //   * GCal Change..:  5.0
  //   * Exposure Time:  1.0
  //   * Readout......: 41.1
  //   * Writeout.....: 10.0
  //                    ----
  //                    57.1

  // * Science (1):
  //   * Science Fold:    15.0
  //   * Exposure Time: 1200.0
  //   * Readout......:   41.1
  //   * Writeout.....:   10.0
  //                      ----
  //                    1266.1

  // * Science (2):
  //   * Offs 0-> 15..:    7.09375 (7 + 0.00625 * 15)
  //   * Exposure Time: 1200.0
  //   * Readout......:   41.1
  //   * Writeout.....:   10.0
  //                      ----
  //                    1258.19375

  // * Science (3):
  //   * Offs 15-> -15:    7.1875 (7 + 0.00625 * 30)
  //   * Exposure Time: 1200.0
  //   * Readout......:   41.1
  //   * Writeout.....:   10.0
  //                      ----
  //                    1258.2875

  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  // 4 atoms, each with an arc and a flat
  val CalibrationTime: BigDecimal =
    ("67.1".sec * 4) + ("57.1".sec * 4)

  // 4 atoms, all of which incur the 1266.1 cost including the science fold
  // move, 3 of them have an additional 2 steps
  val ScienceTime: BigDecimal =
    ("1266.1".sec * 4) + ("1258.19375".sec + "1258.2875".sec) * 3

  val ProgramTime: BigDecimal = CalibrationTime + ScienceTime

  def digestQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            digest {
              state
              value {
                setup {
                  full { seconds }
                  reacquisition { seconds }
                }
                science {
                  observeClass
                  timeEstimate {
                    program { seconds }
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
      }
    """

  val successDigestResult: Json =
    json"""
      {
        "observation": {
          "execution": {
            "digest": {
              "state": "READY",
              "value": {
                "setup" : {
                  "full" : {
                    "seconds" : 960.000000
                  },
                  "reacquisition" : {
                    "seconds" : 300.000000
                  }
                },
                "science" : {
                  "observeClass" : "SCIENCE",
                  "timeEstimate" : {
                    "program" : {
                      "seconds" : ${ProgramTime.asJson}
                    },
                    "nonCharged" : {
                      "seconds" : 0.000000
                    },
                    "total" : {
                      "seconds" : ${ProgramTime.asJson}
                    }
                  },
                  "offsets" : [
                    {
                      "p" : {
                        "arcseconds" : 0.000000
                      },
                      "q" : {
                        "arcseconds" : -15.000000
                      }
                    },
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
                  "atomCount": 4
                }
              }
            }
          }
        }
      }
    """

  test("digest - pending first calculation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = digestQuery(oid),
        expected = List(s"The background calculation has not (yet) produced a value for observation $oid").asLeft
      )

  test("digest - point band normalized"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = digestQuery(oid),
        expected = successDigestResult.asRight
      )

  test("digest - point emission lines"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p, PointEmissionLinesProfile)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = digestQuery(oid),
        expected = successDigestResult.asRight
      )

  test("digest - uniform emission lines"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p, UniformEmissionLinesProfile)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = digestQuery(oid),
        expected = successDigestResult.asRight
      )

  test("digest: deleted target"):

    val setup: IO[(Program.Id, Observation.Id)] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- deleteTargetAs(pi, t)
        _ <- runObscalcUpdate(p, o)
      yield (p, o)

    setup.flatMap: (pid, oid) =>
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
                         state
                         value {
                           setup {
                             full { seconds }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected =
          Ior.both(
            List(s"Could not generate a sequence for $oid: observation is missing target"),
            json"""
              {
                "program": {
                  "observations": {
                    "matches": [
                      {
                        "id": $oid,
                        "execution": {
                          "digest": {
                            "state": "READY",
                            "value": null
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

  test("digest: one bad"):

    val setup: IO[(Program.Id, Observation.Id)] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationWithNoModeAs(pi, p, t)
        _ <- runObscalcUpdate(p, o)
      yield (p, o)

    setup.flatMap: (pid, oid) =>
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
                         state
                         value {
                           setup {
                             full { seconds }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"Could not generate a sequence for $oid: observation is missing observing mode"),
          json"""
            {
              "program": {
                "observations": {
                  "matches": [
                    {
                      "id": $oid,
                      "execution": {
                        "digest": {
                          "state": "READY",
                          "value": null
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

  test("digest: one good, one bad"):

    val setup: IO[(Program.Id, Observation.Id, Observation.Id)] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        o1 <- createObservationWithNoModeAs(pi, p, t)
        _ <- runObscalcUpdate(p, o0)
        _ <- runObscalcUpdate(p, o1)
      yield (p, o0, o1)

    setup.flatMap: (pid, oid0, oid1) =>
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
                         value {
                           setup {
                             full { seconds }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"Could not generate a sequence for $oid1: observation is missing observing mode"),
          json"""
            {
              "program": {
                "observations": {
                  "matches": [
                    {
                      "id": $oid0,
                      "execution": {
                        "digest": {
                          "value": {
                            "setup" : {
                              "full" : {
                                "seconds" : 960.000000
                              }
                            }
                          }
                        }
                      }
                    },
                    {
                      "id": $oid1,
                      "execution": {
                        "digest": {
                          "value": null
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

  test("digest: one bad, one good"):

    val setup: IO[(Program.Id, Observation.Id, Observation.Id)] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o0 <- createObservationWithNoModeAs(pi, p, t)
        o1 <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o0)
        _ <- runObscalcUpdate(p, o1)
      yield (p, o0, o1)

    setup.flatMap: (pid, oid0, oid1) =>
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
                         value {
                           setup {
                             full { seconds }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"Could not generate a sequence for $oid0: observation is missing observing mode"),
          json"""
            {
              "program": {
                "observations": {
                  "matches": [
                    {
                      "id": $oid0,
                      "execution": {
                        "digest": {
                          "value": null
                        }
                      }
                    },
                    {
                      "id": $oid1,
                      "execution": {
                        "digest": {
                          "value": {
                            "setup" : {
                              "full" : {
                                "seconds" : 960.000000
                              }
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

  test("clear execution digest"):

    val setup: IO[(Program.Id, Observation.Id, Step.Id)] =
      import lucuma.odb.json.all.transport.given

      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v)
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled))
      yield (p, o, s)

    val isEmpty = setup.flatMap:
      case (p, o, s) =>
        withServices(pi): services =>
          services.session.transaction.use: xa =>
            for
              _ <- services.executionDigestService.insertOrUpdate(p, o, Md5Hash.Zero, ExecutionDigest.Zero)(using xa)
              _ <- services.executionEventService.insertStepEvent(AddStepEventInput(s, StepStage.EndStep, None))(using xa, ().asInstanceOf) // shhh
              d <- services.executionDigestService.selectOne(o, Md5Hash.Zero)(using xa)
            yield d.isEmpty

    assertIOBoolean(isEmpty, "The execution digest should be removed")


  def executionStateQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution { digest { value { science { executionState } } } }
        }
      }
    """

  def expectedExecutionState(e: ExecutionState): Json =
    json"""
      {
        "observation": {
          "execution": {
            "digest": {
              "value": {
                "science" : {
                  "executionState": ${e.tag.toScreamingSnakeCase}
                }
              }
            }
          }
        }
      }
    """

  import lucuma.odb.json.all.transport.given

  test("executionState - NOT_STARTED"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionStateQuery(oid),
        expected = expectedExecutionState(ExecutionState.NotStarted).asRight
      )

  test("executionState - ONGOING"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        // We now need to record at least a single step to count as ONGOING
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        _ <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionStateQuery(oid),
        expected = expectedExecutionState(ExecutionState.Ongoing).asRight
      )

  test("executionState - DECLARED_COMPLETE"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        _  <- computeItcResultAs(pi, o)
        _  <- setObservationWorkflowState(pi, o, ObservationWorkflowState.Completed)
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionStateQuery(oid),
        expected = expectedExecutionState(ExecutionState.DeclaredComplete).asRight
      )

  test("digest: declared complete"):

    val setup: IO[(Program.Id, Observation.Id)] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(0), ArcStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(0), FlatStep, telescopeConfig(0, 0, StepGuideState.Disabled), ObserveClass.NightCal)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled), ObserveClass.Science)
        _  <- addEndStepEvent(s2)
        _  <- computeItcResultAs(pi, o)
        _  <- setObservationWorkflowState(pi, o, ObservationWorkflowState.Completed)
        _ <- runObscalcUpdate(p, o)
      yield (p, o)

    setup.flatMap: (_, oid) =>
      expect(
        user     = pi,
        query    = digestQuery(oid),
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "digest": {
                    "state": "READY",
                    "value": {
                      "setup" : {
                        "full" : {
                          "seconds" : 0.000000
                        },
                        "reacquisition" : {
                          "seconds" : 0.000000
                        }
                      },
                      "science" : {
                        "observeClass" : "DAY_CAL",
                        "timeEstimate" : {
                          "program" : {
                            "seconds" : 0.000000
                          },
                          "nonCharged" : {
                            "seconds" : 0.000000
                          },
                          "total" : {
                            "seconds" : 0.000000
                          }
                        },
                        "offsets" : [],
                        "atomCount": 0
                      }
                    }
                  }
                }
              }
            }
          """.asRight
        )

  test("executionState - COMPLETED"):
    def atom(v: Visit.Id, ditherNm: Int, q0: Int, qs: Int*): IO[Unit] =
      for
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        c <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(ditherNm), ArcStep, gcalTelescopeConfig(q0), ObserveClass.NightCal)
        _ <- addEndStepEvent(c)
        f <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(ditherNm), FlatStep, gcalTelescopeConfig(q0), ObserveClass.NightCal)
        _ <- addEndStepEvent(f)
        s <- (q0 :: qs.toList).traverse(q => recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(ditherNm), StepConfig.Science, sciTelescopeConfig(q), ObserveClass.Science))
        _ <- s.traverse(addEndStepEvent)
      yield ()

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        _ <- atom(v,  0, 0, 15, -15)
        _ <- atom(v,  5, 0, 15, -15)
        _ <- atom(v, -5, 0, 15, -15)
        _ <- atom(v,  0, 0)
        _ <- runObscalcUpdate(p, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionStateQuery(oid),
        expected = expectedExecutionState(ExecutionState.Completed).asRight
      )

  // This simulates a deleted calibration observation and checks that the foreign key violation is caught
  test("insertOrUpdate with non-existent observation should not fail"):
    createProgramAs(pi).flatMap: pid =>
      withServices(pi): services =>
        Services.asSuperUser:
          services.session.transaction.use: xa =>
            services.executionDigestService
              .insertOrUpdate(
                pid,
                Observation.Id.fromLong(Long.MaxValue).get,
                Md5Hash.Zero,
                ExecutionDigest.Zero
              )(using xa)
}
