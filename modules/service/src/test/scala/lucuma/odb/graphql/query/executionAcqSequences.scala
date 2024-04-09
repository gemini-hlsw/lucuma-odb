// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.odb.json.all.transport.given

class executionAcqSequences extends ExecutionTestSupport {

  test("initial generation") {
    val setup: IO[Observation.Id] =
      for {
        p <- createProgram
        t <- createTargetWithProfileAs(user, p)
        o <- createGmosNorthLongSlitObservationAs(user, p, List(t))
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
                       acquisition {
                         nextAtom {
                           $GmosScienceAtomQuery
                         }
                         possibleFuture {
                           $GmosScienceAtomQuery
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
                      "acquisition": {
                        "nextAtom": {
                          "steps": [
                            ${StepConfigScienceP00Q00Json.deepMerge(GmosNorthAcq0Json)},
                            ${StepConfigScienceP10Q00Json.deepMerge(GmosNorthAcq1Json)},
                            ${StepConfigScienceP00Q00Json.deepMerge(GmosNorthAcq2Json)}
                          ]
                        },
                        "possibleFuture": []
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

  test("execute first atom - repeat of last acq step") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o  <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        v  <- recordVisitAs(user, Instrument.GmosNorth, o)

        // Record the first atom with 3 steps
        a  <- recordAtomAs(user, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthAcq0, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthAcq1, ScienceP10Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(user, a, Instrument.GmosNorth, GmosNorthAcq2, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Now the last acquisition step should be generated as the nextAtom
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
                       acquisition {
                         nextAtom {
                           $GmosScienceAtomQuery
                         }
                         possibleFuture {
                           $GmosScienceAtomQuery
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
                      "acquisition": {
                        "nextAtom": {
                          "steps": [
                            ${StepConfigScienceP00Q00Json.deepMerge(GmosNorthAcq2Json)}
                          ]
                        },
                        "possibleFuture": []
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

  test("execute first and second atoms - 2nd repeat of last acq step") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o  <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        v  <- recordVisitAs(user, Instrument.GmosNorth, o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(user, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthAcq0, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthAcq1, ScienceP10Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthAcq2, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Second atom with just the last acq step
        a1 <- recordAtomAs(user, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 1)
        s3 <- recordStepAs(user, a1, Instrument.GmosNorth, GmosNorthAcq2, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Now we should expect to generate (again) the last acq step
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
                       acquisition {
                         nextAtom {
                           $GmosScienceAtomQuery
                         }
                         possibleFuture {
                           $GmosScienceAtomQuery
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
                      "acquisition": {
                        "nextAtom": {
                          "steps": [
                            ${StepConfigScienceP00Q00Json.deepMerge(GmosNorthAcq2Json)}
                          ]
                        },
                        "possibleFuture": []
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

  test("execute acquisition, switch to science, back to acquisition") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(user, p)
        o  <- createGmosNorthLongSlitObservationAs(user, p, List(t))
        v  <- recordVisitAs(user, Instrument.GmosNorth, o)

        // Acquisition Sequence
        a0 <- recordAtomAs(user, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthAcq0, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthAcq1, ScienceP10Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(user, a0, Instrument.GmosNorth, GmosNorthAcq2, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)
        a1 <- recordAtomAs(user, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 1)
        s3 <- recordStepAs(user, a1, Instrument.GmosNorth, GmosNorthAcq2, ScienceP00Q00, ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Do a science step
        a2 <- recordAtomAs(user, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 1)
        s4 <- recordStepAs(user, a2, Instrument.GmosNorth, GmosNorthScience0, ScienceP10Q00, ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        // Now when we ask for acquisition, we should expect to take it from the top.
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
                       acquisition {
                         nextAtom {
                           $GmosScienceAtomQuery
                         }
                         possibleFuture {
                           $GmosScienceAtomQuery
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
                      "acquisition": {
                        "nextAtom": {
                          "steps": [
                            ${StepConfigScienceP00Q00Json.deepMerge(GmosNorthAcq0Json)},
                            ${StepConfigScienceP10Q00Json.deepMerge(GmosNorthAcq1Json)},
                            ${StepConfigScienceP00Q00Json.deepMerge(GmosNorthAcq2Json)}
                          ]
                        },
                        "possibleFuture": []
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

}
