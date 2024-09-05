// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.odb.json.all.transport.given

class executionAcq extends ExecutionTestSupport {

  val InitialAcquisition: Json =
    json"""
      {
        "observation": {
          "execution": {
            "config": {
              "gmosNorth": {
                "acquisition": {
                  "nextAtom": {
                    "description": "Initial Acquisition",
                    "observeClass": "ACQUISITION",
                    "steps": [
                      ${gmosNorthExpectedAcq(0,  0)},
                      ${gmosNorthExpectedAcq(1, 10)},
                      ${gmosNorthExpectedAcq(2,  0)}
                    ]
                  },
                  "possibleFuture": [
                    {
                      "description": "Fine Adjustments",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${gmosNorthExpectedAcq(2, 0)}
                      ]
                    }
                  ],
                  "hasMore": false
                }
              }
            }
          }
        }
      }
    """

  val FineAdjustments: Json =
    json"""
      {
        "observation": {
          "execution": {
            "config": {
              "gmosNorth": {
                "acquisition": {
                  "nextAtom": {
                    "description": "Fine Adjustments",
                    "observeClass": "ACQUISITION",
                    "steps": [
                      ${gmosNorthExpectedAcq(2, 0)}
                    ]
                  },
                  "possibleFuture": [
                    {
                      "description": "Fine Adjustments",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${gmosNorthExpectedAcq(2, 0)}
                      ]
                    }
                  ],
                  "hasMore": false
                }
              }
            }
          }
        }
      }
    """


  test("initial generation") {
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
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected = InitialAcquisition.asRight
      )
    }
  }

  test("execute first atom - repeat of last acq step") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom with 3 steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(1), scienceStep(10, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Now the last acquisition step should be generated as the nextAtom
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected = FineAdjustments.asRight
      )
    }
  }

  test("execute first step only") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom and one of its steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)

      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "acquisition": {
                        "nextAtom": {
                          "description": "Initial Acquisition",
                          "observeClass": "ACQUISITION",
                          "steps": [
                            ${gmosNorthExpectedAcq(1, 10)},
                            ${gmosNorthExpectedAcq(2,  0)}
                          ]
                        },
                        "possibleFuture": [
                          {
                            "description": "Fine Adjustments",
                            "observeClass": "ACQUISITION",
                            "steps": [
                              ${gmosNorthExpectedAcq(2, 0)}
                            ]
                          }
                        ],
                        "hasMore": false
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

  test("execute first and second atoms - 2nd repeat of last acq step") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), scienceStep(10, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Second atom with just the last acq step
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 1)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Now we should expect to generate (again) the last acq step
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected = FineAdjustments.asRight
      )
    }
  }

  test("execute acquisition, switch to science, back to acquisition") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Acquisition Sequence
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), scienceStep(10, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 1)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Do a science step
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science, stepCount = 1)
        s4 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthScience(0), scienceStep(0, 0), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        // Now when we ask for acquisition, we should expect to take it from the top.
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected = InitialAcquisition.asRight
      )
    }
  }

  // TODO: SEQUENCE UPDATE
  //
  // We query on step records, but that requires an actual step in order to
  // realize that the visit has changed.  I'll need to right join on visits and
  // record(step | visit) or something.
  //
  test("execute acquisition, make a new visit, back to acquisition".ignore) {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Acquisition Sequence
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), scienceStep(10, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Acquisition, stepCount = 1)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Record a new visit, but don't execute anything
        _  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Now when we ask for acquisition, we should expect to take it from the top.
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected = InitialAcquisition.asRight
      )
    }
  }

  test("execute first step, second step, fail second step only") {
    val setup: IO[Observation.Id] =
      for {
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom and two of its steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition, stepCount = 3)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), scienceStep(0, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(1), scienceStep(10, 0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)

        // Fail the second step
        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)

        // We'll have to repeat the second step (index 1)
      } yield o

    setup.flatMap { oid =>
      expect(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 ${excutionConfigQuery("gmosNorth", "acquisition", GmosAtomQuery, None)}
               }
             }
           """,
        expected =
          json"""
            {
              "observation": {
                "execution": {
                  "config": {
                    "gmosNorth": {
                      "acquisition": {
                        "nextAtom": {
                          "description": "Initial Acquisition",
                          "observeClass": "ACQUISITION",
                          "steps": [
                            ${gmosNorthExpectedAcq(1, 10)},
                            ${gmosNorthExpectedAcq(2,  0)}
                          ]
                        },
                        "possibleFuture": [
                          {
                            "description": "Fine Adjustments",
                            "observeClass": "ACQUISITION",
                            "steps": [
                              ${gmosNorthExpectedAcq(2, 0)}
                            ]
                          }
                        ],
                        "hasMore": false
                      }
                    }
                  }
                }
              }
            }
          """.asRight
      )
    }
  }

}
