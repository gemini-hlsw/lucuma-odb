// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*

class replaceGmosNorthSequence extends query.ExecutionTestSupportForGmos:

  def stepInput(filter: GmosNorthFilter): String =
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
                grating: R831_G5302
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