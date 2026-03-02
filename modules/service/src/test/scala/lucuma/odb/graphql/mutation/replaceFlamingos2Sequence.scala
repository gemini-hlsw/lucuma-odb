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

class replaceFlamingos2Sequence extends query.ExecutionTestSupportForFlamingos2:
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

  def atomInput(description: String, steps: String*): String =
    s"""
        {
          description: "$description"
          steps: ${steps.mkString("[\n", ",\n", "]\n")}
        }
    """

  def input(
    oid:          Observation.Id,
    sequenceType: SequenceType,
    atoms:        String*
  ): String =
    s"""
      {
        observationId: "$oid"
        sequenceType:  ${sequenceType.tag.toScreamingSnakeCase}
        sequence: ${atoms.mkString("[\n", ",\n", "]\n")}
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

  def mutation(input: String): String =
    s"""
      mutation {
        replaceFlamingos2Sequence(input: $input) {
          sequence {
            id
            steps { id }
          }
        }
      }
    """

  def mutationOutput(json: Json): List[(Atom.Id, List[Step.Id])] =
    json
      .hcursor
      .downFields("replaceFlamingos2Sequence", "sequence")
      .values
      .toList
      .flatMap(_.toList)
      .map: atomJson =>
        val c       = atomJson.hcursor
        val atomId  = c.downField("id").require[Atom.Id]
        val stepIds =
          c.downField("steps")
            .values
            .toList
            .flatMap(_.toList)
            .map(_.hcursor.downField("id").require[Step.Id])
        (atomId, stepIds)


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
        i0 <- query(pi, mutation(in)).map(mutationOutput)
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
        i0 <- query(pi, mutation(in)).map(mutationOutput)
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
        mutation(in),
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
        r  <- query(staff, mutation(in))
      yield mutationOutput(r).nonEmpty

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
      _  <- expect(pi, mutation(in), List(
        "Execution sequences containing over 1000 atoms are not supported."
      ).asLeft)
    yield ()