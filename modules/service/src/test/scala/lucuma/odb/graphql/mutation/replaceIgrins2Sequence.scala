// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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

class replaceIgrins2Sequence extends query.ExecutionTestSupportForIgrins2 with ReplaceSequenceOps:
  def stepInput(exposureSeconds: BigDecimal): String =
    s"""
          {
            instrumentConfig: {
              exposure: {
                seconds: $exposureSeconds
              }
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
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(20)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceIgrins2Sequence(input: $inputString) {
              sequence {
                description
                steps {
                  instrumentConfig {
                    exposure { seconds }
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "replaceIgrins2Sequence": {
              "sequence": [
                {
                  "description": "Foo",
                  "steps": [
                    {
                      "instrumentConfig": {
                        "exposure": { "seconds": 20.000000 }
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
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceIgrins2Sequence(input: {
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
            "replaceIgrins2Sequence": {
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
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
        i0 <- query(pi, mutation(Instrument.Igrins2, in)).map(mutationOutput(Instrument.Igrins2, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("Matches execution config (after first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        _ <- recordVisitAs(serviceUser, Instrument.Igrins2, o)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
        i0 <- query(pi, mutation(Instrument.Igrins2, in)).map(mutationOutput(Instrument.Igrins2, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("PI cannot edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        v <- recordVisitAs(serviceUser, Instrument.Igrins2, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
      _  <- expect(
        pi,
        mutation(Instrument.Igrins2, in),
        List(
          s"Observation $o is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Inactive/Completed).",
          "User cannot replace the sequence in the current observation workflow state."
        ).asLeft
      )
    yield ()

  test("Staff can edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
        v <- recordVisitAs(serviceUser, Instrument.Igrins2, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(20)))
        r  <- query(staff, mutation(Instrument.Igrins2, in))
      yield mutationOutput(Instrument.Igrins2, r).nonEmpty

  test("Can't add too many atoms"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createIgrins2LongSlitObservationAs(pi, p, t)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, List.fill(1001)(atomInput("Foo", stepInput(20)))*)
      _  <- expect(pi, mutation(Instrument.Igrins2, in), List(
        "Execution sequences containing over 1000 atoms are not supported."
      ).asLeft)
    yield ()
