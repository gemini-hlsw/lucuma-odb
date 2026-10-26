// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.foldable.*
import cats.syntax.option.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2WindowCover
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*

class replaceFlamingos2Sequence extends query.ExecutionTestSupportForFlamingos2 with ReplaceSequenceOps:
  def stepInput(
    filter:      Flamingos2Filter,
    windowCover: Option[Flamingos2WindowCover] = None,
    stepConfig:  String                        = "science: true"
  ): String =
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
              ${windowCover.fold("")(w => s"windowCover: ${w.tag.toScreamingSnakeCase}")}
            }
            stepConfig: {
              $stepConfig
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

  test("Empty"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceFlamingos2Sequence(input: {
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
            "replaceFlamingos2Sequence": {
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
        _ <- recordVisitAs(serviceUser, o)
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
        v <- recordVisitAs(serviceUser, o)
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
          s"Observation $o is ineligible for this operation due to its workflow state (Ongoing with allowed transition to Completed).",
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
        v <- recordVisitAs(serviceUser, o)
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

  private def replaceSequenceQuery(inputString: String): String =
    s"""
      mutation {
        replaceFlamingos2Sequence(input: $inputString) {
          sequence {
            description
          }
        }
      }
    """

  test("no recorded static config - MOS"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2MosObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Mos", stepInput(Flamingos2Filter.J)))
      expect(
        user     = pi,
        query    = replaceSequenceQuery(inputString),
        expected = json"""
          {
            "replaceFlamingos2Sequence": {
              "sequence": [ { "description": "Mos" } ]
            }
          }
        """.asRight
      )

  test("no recorded static config - imaging"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2ImagingObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Img", stepInput(Flamingos2Filter.J)))
      expect(
        user     = pi,
        query    = replaceSequenceQuery(inputString),
        expected = json"""
          {
            "replaceFlamingos2Sequence": {
              "sequence": [ { "description": "Img" } ]
            }
          }
        """.asRight
      )

  test("rejects a non-Flamingos 2 observation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
      expect(
        user     = pi,
        query    = replaceSequenceQuery(inputString),
        expected = List(s"Observation $oid not found or is not a Flamingos 2 observation.").asLeft
      )

  test("rejects an observation with no observing mode"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createObservationAs(pi, p, t)
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
      expect(
        user     = pi,
        query    = replaceSequenceQuery(inputString),
        expected = List(s"Observation $oid not found or is not a Flamingos 2 observation.").asLeft
      )

  private def windowCoverQuery(inputString: String): String =
    s"""
      mutation {
        replaceFlamingos2Sequence(input: $inputString) {
          sequence {
            steps {
              instrumentConfig {
                windowCover
              }
            }
          }
        }
      }
    """

  private def windowCoverResult(windowCover: Option[Flamingos2WindowCover]): Json =
    json"""
      {
        "replaceFlamingos2Sequence": {
          "sequence": [
            {
              "steps": [
                {
                  "instrumentConfig": {
                    "windowCover": ${windowCover.map(_.tag.toScreamingSnakeCase)}
                  }
                }
              ]
            }
          ]
        }
      }
    """

  private def createWindowCoverObservation: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
    yield o

  test("window cover defaults to null"):
    createWindowCoverObservation.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J)))
      expect(pi, windowCoverQuery(inputString), windowCoverResult(none).asRight)

  test("window cover round trips"):
    Flamingos2WindowCover.values.toList.traverse_ : wc =>
      createWindowCoverObservation.flatMap: oid =>
        val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(Flamingos2Filter.J, wc.some)))
        expect(pi, windowCoverQuery(inputString), windowCoverResult(wc.some).asRight)
