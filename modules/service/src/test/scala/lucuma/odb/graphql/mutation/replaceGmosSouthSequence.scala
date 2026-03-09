// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.eq.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*

class replaceGmosSouthSequence extends query.ExecutionTestSupportForGmos with ReplaceSequenceOps:

  def stepInput(filter: GmosSouthFilter): String =
    s"""
          {
            instrumentConfig: {
              exposure: {
                seconds: 20
              }
              readout: {
                xBin: ONE
                yBin: ONE
                ampCount: TWELVE
                ampGain: LOW
                ampReadMode: SLOW
              }
              dtax: ZERO
              roi: FULL_FRAME
              gratingConfig: {
                grating: R600_G5324
                order: ZERO
                wavelength: {
                  nanometers: 500.0
                }
              }
              filter: ${filter.tag.toScreamingSnakeCase}
              fpu: {
                builtin: LONG_SLIT_0_50
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
        o <- createGmosSouthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      val inputString = input(oid, SequenceType.Science, atomInput("Foo", stepInput(GmosSouthFilter.GPrime)))
      expect(
        user     = pi,
        query    = s"""
          mutation {
            replaceGmosSouthSequence(input: $inputString) {
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
            "replaceGmosSouthSequence": {
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
        o <- createGmosSouthLongSlitObservationAs(pi, p, List(t))
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(GmosSouthFilter.GPrime)))
        i0 <- query(pi, mutation(Instrument.GmosSouth, in)).map(mutationOutput(Instrument.GmosSouth, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1

  test("Matches execution config (after first visit)"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosSouthLongSlitObservationAs(pi, p, List(t))
        _ <- recordVisitAs(serviceUser, Instrument.GmosSouth, o)
      yield o

    assertIOBoolean:
      for
        o  <- setup
        in  = input(o, SequenceType.Science, atomInput("Foo", stepInput(GmosSouthFilter.GPrime)))
        i0 <- query(pi, mutation(Instrument.GmosSouth, in)).map(mutationOutput(Instrument.GmosSouth, _))
        i1 <- scienceSequenceIds(pi, o).map(_.toList)
      yield i0 === i1