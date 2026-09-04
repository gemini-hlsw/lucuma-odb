// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import io.circe.literal.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target

class replaceGhostSequence extends query.ExecutionTestSupportForGhost with ReplaceSequenceOps:

  val mode: String =
    """
      ghostIfu: {
        stepCount: 1
        resolutionMode: STANDARD
        red: {
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 10.0 }
              count: 2
              at: { nanometers: 500 }
            }
          }
        }
        blue: {
          exposureTimeMode: {
            timeAndCount: {
              time: { seconds: 30.0 }
              count: 4
              at: { nanometers: 500 }
            }
          }
        }
        slitViewingCameraExposureTime: { seconds: 5.0 }
      }
    """

  def createGhostObservation(p: Program.Id, t: Target.Id): IO[Observation.Id] =
    createObservationWithModeAs(pi, p, List(t), mode)

  def stepInput(redSeconds: BigDecimal, blueSeconds: BigDecimal): String =
    s"""
          {
            instrumentConfig: {
              red: {
                exposureTime: { seconds: $redSeconds }
                exposureCount: 2
                binning: ONE_BY_TWO
                readMode: MEDIUM
              }
              blue: {
                exposureTime: { seconds: $blueSeconds }
                exposureCount: 4
                binning: ONE_BY_ONE
                readMode: FAST
              }
              ifu1FiberAgitator: ENABLED
              ifu2FiberAgitator: DISABLED
            }
            stepConfig: {
              science: true
            }
            observeClass: SCIENCE
          }
    """

  test("Simple one atom, one step"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(20, 40)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGhostSequence(input: $inputString) {
              sequence {
                description
                steps {
                  instrumentConfig {
                    red {
                      exposureTime { seconds }
                      exposureCount
                      binning
                      readMode
                    }
                    blue {
                      exposureTime { seconds }
                      exposureCount
                      binning
                      readMode
                    }
                    ifu1FiberAgitator
                    ifu2FiberAgitator
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "replaceGhostSequence": {
              "sequence": [
                {
                  "description": "Foo",
                  "steps": [
                    {
                      "instrumentConfig": {
                        "red": {
                          "exposureTime": { "seconds": 20.000000 },
                          "exposureCount": 2,
                          "binning": "ONE_BY_TWO",
                          "readMode": "MEDIUM"
                        },
                        "blue": {
                          "exposureTime": { "seconds": 40.000000 },
                          "exposureCount": 4,
                          "binning": "ONE_BY_ONE",
                          "readMode": "FAST"
                        },
                        "ifu1FiberAgitator": "ENABLED",
                        "ifu2FiberAgitator": "DISABLED"
                      }
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )

  test("Empty"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGhostSequence(input: {
              observationId: "$oid"
              sequenceType: SCIENCE
              sequence: []
            }) {
              sequence {
                description
              }
            }
          }
        """,
        expected = json"""
          {
            "replaceGhostSequence": {
              "sequence": []
            }
          }
        """.asRight
      )

  test("Matches execution config (before first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20, 40)))
        i0 <- query(pi, mutation(Instrument.Ghost, in)).map(mutationOutput(Instrument.Ghost, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("Matches execution config (after first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
        _ <- recordVisitAs(serviceUser, o)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20, 40)))
        i0 <- query(pi, mutation(Instrument.Ghost, in)).map(mutationOutput(Instrument.Ghost, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("PI cannot edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
        v <- recordVisitAs(serviceUser, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20, 40)))
      _  <- expect(
        pi,
        mutation(Instrument.Ghost, in),
        List(
          s"Observation $o is ineligible for this operation due to its workflow state (Completed).",
          "User cannot replace the sequence in the current observation workflow state."
        ).asLeft
      )
    yield ()

  test("Staff can edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
        v <- recordVisitAs(serviceUser, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20, 40)))
        r  <- query(staff, mutation(Instrument.Ghost, in))
      yield mutationOutput(Instrument.Ghost, r).nonEmpty

  test("Can't add too many atoms"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGhostObservation(p, t)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, List.fill(1001)(atomInput("Foo", stepInput(20, 40)))*)
      _  <- expect(pi, mutation(Instrument.Ghost, in), List(
        "Execution sequences containing over 1000 atoms are not supported."
      ).asLeft)
    yield ()

