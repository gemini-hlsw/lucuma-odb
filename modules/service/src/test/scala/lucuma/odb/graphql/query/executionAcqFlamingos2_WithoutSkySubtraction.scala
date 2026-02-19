// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.traverse.*
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
              "possibleFuture": [],
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
        s  <- firstAcquisitionStepId(serviceUser, o)
        _  <- addEndStepEvent(s, v)
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  test("execute first atom"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Record the first atom with 3 steps
        i <- firstAcquisitionAtomStepIds(serviceUser, o)
        _ <- i.traverse(sid => addEndStepEvent(sid, v))
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
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Record the first atom and one of its steps
        s <- firstAcquisitionStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
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

  test("execute acquisition, reset"):
    val setup: IO[Observation.Id] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        // Acquisition Sequence
        i  <- acquisitionStepIds(serviceUser, o)
        _  <- i.traverse(sid => addEndStepEvent(sid, v))

        // Reset acquisition to take it from the top.
        _  <- resetAcquisitionAs(serviceUser, o)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = flamingos2AcquisitionQuery(oid),
        expected = InitialAcquisition.asRight
      )

  def nextAtomStepIds(o: Observation.Id): IO[NonEmptyList[Step.Id]] =
    acquisitionSequenceIds(serviceUser, o).map(m => NonEmptyList.fromListUnsafe(m.head._2))

  test("nextAtom step ids don't change while executing"):
    val execAcq: IO[List[NonEmptyList[Step.Id]]] =
      for
        p  <- createProgram
        t  <- createTargetWithProfileAs(pi, p)
        o  <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v  <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)

        x0 <- nextAtomStepIds(o)

        // First atom with 3 steps.
        i  <- acquisitionStepIds(serviceUser, o)
        _  <- addEndStepEvent(i(0), v)

        x1 <- nextAtomStepIds(o)

        _  <- addEndStepEvent(i(1), v)

        x2 <- nextAtomStepIds(o)
      yield List(x0, x1, x2)

    (execAcq.flatTap: ids =>
      IO.println(s"x0 = ${ids(0).toList.mkString(", ")}") *>
      IO.println(s"x1 = ${ids(1).toList.mkString(", ")}") *>
      IO.println(s"x2 = ${ids(2).toList.mkString(", ")}")) *>
    execAcq.map: ids =>
      ids.zip(ids.tail).foreach: (before, after) =>
        assertEquals(before.tail, after.toList, s"before: $before, after: $after")
