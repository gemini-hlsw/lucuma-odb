// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Breakpoint
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime
import lucuma.odb.json.all.transport.given

class executionAcqFlamingos2_WithoutSkySubtraction extends ExecutionTestSupportForFlamingos2:

  val ExposureTime: TimeSpan = 2.secTimeSpan

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(1))

  val InitialAcquisition: Json =
    json"""
      {
        "executionConfig": {
          "flamingos2": {
            "acquisition": {
              "nextAtom": {
                "description": "Initial Acquisition",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${flamingos2ExpectedAcq(Flamingos2AcqImage, ExposureTime,    0,  0)},
                  ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10,  0)},
                  ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0,  0, Breakpoint.Enabled)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${flamingos2ExpectedAcq(Flamingos2AcqSlit, ExposureTime, 0, 0)}
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
          "flamingos2": {
            "acquisition": {
              "nextAtom": {
                "description": "Fine Adjustments",
                "observeClass": "ACQUISITION",
                "steps": [
                  ${flamingos2ExpectedAcq(Flamingos2AcqSlit, ExposureTime, 0, 0)}
                ]
              },
              "possibleFuture": [
                {
                  "description": "Fine Adjustments",
                  "observeClass": "ACQUISITION",
                  "steps": [
                    ${flamingos2ExpectedAcq(Flamingos2AcqSlit, ExposureTime, 0, 0)}
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
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first step only, reset"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Record the first atom and one of its steps
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first atom - repeat of last acq step"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Record the first atom with 3 steps
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = 10.secTimeSpan), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Now the last acquisition step should be generated as the nextAtom
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = FineAdjustments.asRight
      )

  test("execute first step only"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Record the first atom and one of its steps
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected =
          json"""
            {
              "executionConfig": {
                "flamingos2": {
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10,  0)},
                        ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0,  0, Breakpoint.Enabled)}
                      ]
                    },
                    "possibleFuture": [
                      {
                        "description": "Fine Adjustments",
                        "observeClass": "ACQUISITION",
                        "steps": [
                          ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime, 0,  0)}
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
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = 10.secTimeSpan), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)

        // Second atom with just the last acq step
        a1 <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s3 <- recordStepAs(serviceUser, a1, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Now we should expect to generate (again) the last acq step
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = FineAdjustments.asRight
      )

  test("execute acquisition, reset"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Acquisition Sequence
        a0 <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = 10.secTimeSpan), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)
        s2 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s2)
        a1 <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s3 <- recordStepAs(serviceUser, a1, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s3)

        // Reset acquisition to take it from the top.
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first step, second step, fail second step only"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Record the first atom and two of its steps
        a  <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)
        s1 <- recordStepAs(serviceUser, a, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = 10.secTimeSpan), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)

        // Fail the second step
        d  <- recordDatasetAs(serviceUser, s1, "N20240905S1000.fits")
        _  <- setQaState(d, DatasetQaState.Usable)

        // We'll have to repeat the second step (index 1)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected =
          json"""
            {
              "executionConfig": {
                "flamingos2": {
                  "acquisition": {
                    "nextAtom": {
                      "description": "Initial Acquisition",
                      "observeClass": "ACQUISITION",
                      "steps": [
                        ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  10.secTimeSpan, 10,  0)},
                        ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0,  0, Breakpoint.Enabled)}
                      ]
                    },
                    "possibleFuture": [
                      {
                        "description": "Fine Adjustments",
                        "observeClass": "ACQUISITION",
                        "steps": [
                          ${flamingos2ExpectedAcq(Flamingos2AcqSlit,  ExposureTime,    0,  0)}
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

  def nextAtomStepIds(o: Observation.Id): IO[NonEmptyList[Step.Id]] =
    scienceSequenceIds(serviceUser, o).map(m => NonEmptyList.fromListUnsafe(m.head._2))

  test("nextAtom step ids don't change while executing"):
    val execAcq: IO[List[NonEmptyList[Step.Id]]] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        x0 <- nextAtomStepIds(o)

        // First atom with 3 steps.
        a0 <- recordAtomAs(serviceUser, Instrument.Flamingos2, v, SequenceType.Acquisition)
        s0 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqImage.copy(exposure = ExposureTime), StepConfig.Science, acqTelescopeConfig(0), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s0)

        x1 <- nextAtomStepIds(o)

        s1 <- recordStepAs(serviceUser, a0, Instrument.Flamingos2, Flamingos2AcqSlit.copy(exposure = 10.secTimeSpan), StepConfig.Science, acqTelescopeConfig(10), ObserveClass.Acquisition)
        _  <- addEndStepEvent(s1)

        x2 <- nextAtomStepIds(o)
      yield List(x0, x1, x2)

    execAcq.map: ids =>
      ids.zip(ids.tail).foreach: (before, after) =>
        assertEquals(before.tail, after.toList, s"before: $before, after: $after")
