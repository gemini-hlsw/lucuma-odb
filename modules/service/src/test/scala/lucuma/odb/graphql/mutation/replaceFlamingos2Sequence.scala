// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*

class replaceFlamingos2Sequence extends query.ExecutionTestSupportForFlamingos2 with ReplaceSequenceOps:
  def stepInput(filter: Flamingos2Filter): String =
    s"""
          {
            instrumentConfig: {
              exposure: {
                seconds: 20
              }
              filter: ${filter.tag.toScreamingSnakeCase}
              readMode: BRIGHT
              lyotWheel: F16
              decker: LONG_SLIT
              readoutMode: SCIENCE
              reads: READS_1
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
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceFlamingos2Sequence(input: $inputString) {
              sequence {
                description
                steps {
                  instrumentConfig {
                    filter
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "replaceFlamingos2Sequence": {
              "sequence": [
                {
                  "description": "Foo",
                  "steps": [
                    {
                      "instrumentConfig": { "filter": "J" }
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )

  test("Matches execution config (before first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
        i0 <- query(pi, mutation(Instrument.Flamingos2, in)).map(mutationOutput(Instrument.Flamingos2, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("Matches execution config (after first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        _ <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
        i0 <- query(pi, mutation(Instrument.Flamingos2, in)).map(mutationOutput(Instrument.Flamingos2, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("PI cannot edit the sequence after execution starts"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
      _  <- expect(
        pi,
        mutation(Instrument.Flamingos2, in),
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
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        v <- recordVisitAs(serviceUser, Instrument.Flamingos2, o)
        s <- firstScienceStepId(serviceUser, o)
        _ <- addEndStepEvent(s, v)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
        r  <- query(staff, mutation(Instrument.Flamingos2, in))
      yield mutationOutput(Instrument.Flamingos2, r).nonEmpty

  test("Can't add too many atoms"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    for
      o  <- setup
      in  = input(o, SequenceType.Science, List.fill(1001)(atomInput("Foo", stepInput(Flamingos2Filter.J)))*)
      _  <- expect(pi, mutation(Instrument.Flamingos2, in), List(
        "Execution sequences containing over 1000 atoms are not supported."
      ).asLeft)
    yield ()