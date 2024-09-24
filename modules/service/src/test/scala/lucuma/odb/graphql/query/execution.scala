// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.Ior
import cats.effect.IO
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth
import lucuma.odb.data.Md5Hash
import munit.IgnoreSuite

// TODO: SEQUENCE UPDATE
@IgnoreSuite
class execution extends ExecutionTestSupport {

  // Additional cost of an arc (no sci fold move because we are already doing a flat)
  //  5.0 seconds for the Gcal configuration change (shutter, filter, diffuser)
  //  1.0 second for the arc exposure
  // 10.0 seconds for the write cost
  // 41.1 seconds readout (hamamatsu, 1x2, 12 amp, Low, Slow)
  // ----
  // 57.1 seconds per arc

  // 114.2 additional seconds for the two arcs produced by the sequence

  test("digest") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      } yield o
    setup.flatMap { oid =>
      expect(
        user  = pi,
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
                          "seconds" : 471.800000
                        },
                        "nonCharged" : {
                          "seconds" : 0.000000
                        },
                        "total" : {
                          "seconds" : 883.400000
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
                      "atomCount": 8
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

  test("clear execution digest") {

    val setup: IO[(Program.Id, Observation.Id, Step.Id)] = {
      import lucuma.odb.json.all.transport.given

      for {
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)
        a <- recordAtomAs(serviceUser, Instrument.GmosNorth, v)
        s <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0))
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

}
