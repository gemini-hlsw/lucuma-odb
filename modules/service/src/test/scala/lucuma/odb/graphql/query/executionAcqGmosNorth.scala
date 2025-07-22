// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.json.all.transport.given

class executionAcqGmosNorth extends ExecutionTestSupportForGmos:

  val InitialAcquisition: Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${gmosNorthExpectedAcq(0,  0)},
                  ${gmosNorthExpectedAcq(1, 10)},
                  ${gmosNorthExpectedAcq(2,  0, Breakpoint.Enabled)}
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
    """

  val FineAdjustments: Json =
    json"""
      {
        "executionConfig": {
          "gmosNorth": {
            "acquisition": {
              "nextAtom": {
                "description": "Fine Adjustments",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${gmosNorthExpectedAcq(2, 0, Breakpoint.Disabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${gmosNorthExpectedAcq(2, 0, Breakpoint.Disabled)}
                  ]
                }
              ],
              "hasMore": false
            }
          }
        }
      }
    """

  test("initial generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first step only, reset"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom and one of its steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first atom - repeat of last acq step"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom with 3 steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Now the last acquisition step should be generated as the nextAtom
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = FineAdjustments.asRight
      )

  test("execute first step only"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom and one of its steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${gmosNorthExpectedAcq(1, 10)},
                        ${gmosNorthExpectedAcq(2,  0, Breakpoint.Enabled)}
                      ]
                    },
                    "possibleFuture": [
                      {
                        "description": "Fine Adjustments",
                        "observeClass": "ACQUISITION",
                        "steps": [
                          ${gmosNorthExpectedAcq(2, 0, Breakpoint.Disabled)}
                        ]
                      }
                    ],
                    "hasMore": false
                  }
                }
              }
            }
          """.asRight
      )

  test("execute first and second atoms - 2nd repeat of last acq step"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Second atom with just the last acq step
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Now we should expect to generate (again) the last acq step
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = FineAdjustments.asRight
      )

  test("execute acquisition, switch to science, back to acquisition"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Acquisition Sequence
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Do a science step
        a2 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Science)
        s4 <- recordStepAs(serviceUser, a2, Instrument.GmosNorth, gmosNorthScience(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Science)
        _  <- addEndStepEvent(s4)

        // Reset acquisition to take it from the top.
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute acquisition, make a new visit, back to acquisition"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v0 <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Acquisition Sequence
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v0, SequenceType.Acquisition)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Record a new visit, but don't execute anything
        _  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Reset acquisition
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first step, second step, fail second step only"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        // Record the first atom and two of its steps
        a  <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)

        // Fail the second step
        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)

        // We'll have to repeat the second step (index 1)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = gmosNorthAcquisitionQuery(oid),
        expected =
          json"""
            {
              "executionConfig": {
                "gmosNorth": {
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${gmosNorthExpectedAcq(1, 10)},
                        ${gmosNorthExpectedAcq(2,  0, Breakpoint.Enabled)}
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
          """.asRight
      )

  def firstScienceStepId(p: Program.Id, o: Observation.Id): IO[Step.Id] =
    import lucuma.odb.testsyntax.execution.*
    generateOrFail(p, o, 5.some).map(_.gmosNorthScience.nextAtom.steps.head.id)

  test("science step ids do not change while executing acquisition"):
    val execAcq: IO[Set[Step.Id]] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        x0 <- firstScienceStepId(p, o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)

        x1 <- firstScienceStepId(p, o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)

        x1 <- firstScienceStepId(p, o)

        s2 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        x2 <- firstScienceStepId(p, o)

        // Second atom with just the last acq step
        a1 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s3 <- recordStepAs(serviceUser, a1, Instrument.GmosNorth, gmosNorthAcq(2), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        x3 <- firstScienceStepId(p, o)
      yield Set(x0, x1, x2, x3)

    assertIO(execAcq.map(_.size), 1)

  def nextAtomStepIds(p: Program.Id, o: Observation.Id): IO[NonEmptyList[Step.Id]] =
    import lucuma.odb.testsyntax.execution.*
    generateOrFail(p, o, 5.some).map(_.gmosNorthAcquisition.nextAtom.steps.map(_.id))

  test("nextAtom step ids don't change while executing"):
    val execAcq: IO[List[NonEmptyList[Step.Id]]] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.GmosNorth, o)

        x0 <- nextAtomStepIds(p, o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(serviceUser, Instrument.GmosNorth, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(0), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)

        x1 <- nextAtomStepIds(p, o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.GmosNorth, gmosNorthAcq(1), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)

        x2 <- nextAtomStepIds(p, o)
      yield List(x0, x1, x2)

    execAcq.map: ids =>
      ids.zip(ids.tail).foreach: (before, after) =>
        assertEquals(before.tail, after.toList, s"before: $before, after: $after")

  test("reset can only be done by staff or better"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _ <- expect(
        user  = pi,
        query = s"""
          mutation {
            resetAcquisition(input: {
              observationId: "$o"
            }) {
              observation { id }
            }
          }
        """,
        expected = List(
          s"User ${pi.id} is not authorized to perform this operation."
        ).asLeft
      )
    yield ()