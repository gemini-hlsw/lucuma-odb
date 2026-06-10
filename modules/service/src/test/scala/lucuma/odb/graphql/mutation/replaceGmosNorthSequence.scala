// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.odb.sequence.data.UnsplittableAtom

class replaceGmosNorthSequence extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  test("Simple one atom, one step"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(GmosNorthFilter.GPrime)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGmosNorthSequence(input: $inputString) {
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
            "replaceGmosNorthSequence": {
              "sequence": [
                {
                  "description": "Foo",
                  "steps": [
                    {
                      "instrumentConfig": { "filter": "G_PRIME" }
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
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(GmosNorthFilter.GPrime)))
        i0 <- query(pi, mutation(Instrument.GmosNorth, in)).map(mutationOutput(Instrument.GmosNorth, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("Matches execution config (after first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- recordVisitAs(serviceUser, o)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(GmosNorthFilter.GPrime)))
        i0 <- query(pi, mutation(Instrument.GmosNorth, in)).map(mutationOutput(Instrument.GmosNorth, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("mutating imaging observation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthImagingObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Img", imagingStepInput(GmosNorthFilter.GPrime)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGmosNorthSequence(input: $inputString) {
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
            "replaceGmosNorthSequence": {
              "sequence": [
                {
                  "description": "Img",
                  "steps": [
                    {
                      "instrumentConfig": { "filter": "G_PRIME" }
                    }
                  ]
                }
              ]
            }
          }
        """.asRight
      )

  test("manual sequence for unsplittable observation with multiple atoms"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthImagingObservationAs(pi, p, t)
        _ <- setIsSplittableAs(pi, o, isSplittable = false)
      yield o

    setup.flatMap: oid =>
      val inputString = input(
        oid,
        SequenceType.Science,
        atomInput("Atom1", stepInput(GmosNorthFilter.GPrime)),
        atomInput("Atom2", stepInput(GmosNorthFilter.IPrime))
      )

      expect(
        user  = pi,
        query = s"""
          mutation {
            replaceGmosNorthSequence(input: $inputString) {
              sequence {
                description
              }
            }
          }
        """,
        expected = List("Unsplittable observations may only contain a single atom.").asLeft
      )

  test("manual sequence for unsplittable observation with too many steps"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthImagingObservationAs(pi, p, t)
        _ <- setIsSplittableAs(pi, o, isSplittable = false)
      yield o

    setup.flatMap: oid =>
      val step  = stepInput(GmosNorthFilter.GPrime)
      val steps = List.fill(UnsplittableAtom.StepLimit.value + 1)(step)
      val inputString = input(
        oid,
        SequenceType.Science,
        atomInput("Atom1", steps*)
      )

      expect(
        user  = pi,
        query = s"""
          mutation {
            replaceGmosNorthSequence(input: $inputString) {
              sequence {
                description
              }
            }
          }
        """,
        expected = List(
          s"An unsplittable observation's atom may not contain more than ${UnsplittableAtom.StepLimit.value} steps."
        ).asLeft
      )
