// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
import eu.timepit.refined.types.numeric.NonNegInt
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepGuideState
import lucuma.core.enums.StepStage
import lucuma.core.math.SignalToNoise
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime
import lucuma.odb.data.Md5Hash


class executionDigest extends ExecutionTestSupport {

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      NonNegInt.unsafeFrom(10),
      SignalToNoise.unsafeFromBigDecimalExact(50.0)
    )

  // * Arc:
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
  // (there is an offset cost for some steps but it is subsumed by the
  //  science fold movement)
  //   * Science Fold:    15.0
  //   * Exposure Time: 1200.0
  //   * Readout......:   41.1
  //   * Writeout.....:   10.0
  //                      ----
  //                    1266.1

  // * Science (2 and 3):
  //   * Exposure Time: 1200.0
  //   * Readout......:   41.1
  //   * Writeout.....:   10.0
  //                      ----
  //                    1251.1

  extension (s: String)
    def sec: BigDecimal =
      BigDecimal(s).setScale(6)

  // 4 atoms, each with an arc and a flat
  def PartnerTime: BigDecimal =
    ("67.1".sec * 4) + ("57.1".sec * 4)

  // 4 atoms, all of which incur the 1266.1 cost including the science fold
  // move, 3 of them have an additional 2 steps each of 1251.1
  def ProgramTime: BigDecimal =
    ("1266.1".sec * 4) + ("1251.1".sec * 3 * 2)

  def digestQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution {
            digest {
              setup {
                full { seconds }
                reacquisition { seconds }
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
    """

  val successDigestResult: Json =
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
              "science" : {
                "observeClass" : "SCIENCE",
                "timeEstimate" : {
                  "program" : {
                    "seconds" : ${ProgramTime.asJson}
                  },
                  "partner" : {
                    "seconds" : ${PartnerTime.asJson}
                  },
                  "nonCharged" : {
                    "seconds" : 0.000000
                  },
                  "total" : {
                    "seconds" : ${(ProgramTime + PartnerTime).asJson}
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
    """

  test("digest - point band normalized") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
        query = digestQuery(oid),
        expected = successDigestResult.asRight
      )
    }
  }

  test("digest - point emission lines") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p, PointEmissionLinesProfile)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
        query = digestQuery(oid),
        expected = successDigestResult.asRight
      )
    }
  }

  test("digest - uniform emission lines") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p, UniformEmissionLinesProfile)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
        query = digestQuery(oid),
        expected = successDigestResult.asRight
      )
    }
  }

  test("digest: deleted target") {

    val setup: IO[(Program.Id, Observation.Id)] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- deleteTargetAs(pi, t)
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
          List(s"Could not generate a sequence for the observation $oid: observation is missing target"),
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
          List(s"Could not generate a sequence for the observation $oid: observation is missing observing mode"),
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
          List(s"Could not generate a sequence for the observation $oid1: observation is missing observing mode"),
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
          List(s"Could not generate a sequence for the observation $oid0: observation is missing observing mode"),
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

  test("clear execution digest") {

    val setup: IO[(Program.Id, Observation.Id, Step.Id)] = {
      import lucuma.odb.json.all.transport.given

      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v)
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, telescopeConfig(0, 0, StepGuideState.Enabled))
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

  def executionStateQuery(oid: Observation.Id): String =
    s"""
      query {
        observation(observationId: "$oid") {
          execution { digest { science { executionState } } }
        }
      }
    """

  def expectedExecutionState(e: ExecutionState): Json =
    json"""
      {
        "observation": {
          "execution": {
            "digest": {
              "science" : {
                "executionState": ${e.tag.toScreamingSnakeCase}
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
        _  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionStateQuery(oid),
        expected = expectedExecutionState(ExecutionState.Ongoing).asRight
      )

  def gcalTelescopeConfig(q: Int): TelescopeConfig =
    telescopeConfig(0, q, StepGuideState.Disabled)

  def sciTelescopeConfig(q: Int): TelescopeConfig =
    telescopeConfig(0, q, StepGuideState.Enabled)

  test("executionState - COMPLETED"):
    def atom(v: Visit.Id, ditherNm: Int, q: Int, n: Int): IO[Unit] =
      for
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        c <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthArc(ditherNm), ArcStep, gcalTelescopeConfig(q), ObserveClass.PartnerCal)
        _ <- addEndStepEvent(c)
        f <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthFlat(ditherNm), FlatStep, gcalTelescopeConfig(q), ObserveClass.PartnerCal)
        _ <- addEndStepEvent(f)
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(ditherNm), StepConfig.Science, sciTelescopeConfig(q), ObserveClass.Science).replicateA(3)
        _ <- s.traverse(addEndStepEvent)
      yield ()

    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        _ <- atom(v,  0,   0, 3)
        _ <- atom(v,  5,  15, 3)
        _ <- atom(v, -5, -15, 3)
        _ <- atom(v,  0,   0, 1)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionStateQuery(oid),
        expected = expectedExecutionState(ExecutionState.Completed).asRight
      )
}
