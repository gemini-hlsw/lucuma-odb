// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

/**
 * GMOS North MOS science generation.
 * The sequence is the same as for longslit but with the custom mask for fpu.
 */
class executionSciGmosSouthMos extends ExecutionTestSupportForGmos:

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(
      20.minTimeSpan,
      PosInt.unsafeFrom(10)
    )

  private val FpuAtomQuery: String =
    """
      steps {
        instrumentConfig {
          fpu {
            builtin
            customMask { slitWidth }
          }
        }
        stepConfig { stepType }
      }
    """

  private def expectedStep(stepType: String): Json =
    // no need for attachment id at this stage
    json"""
      {
        "instrumentConfig": {
          "fpu": {
            "builtin": null,
            "customMask": { "slitWidth": "CUSTOM_WIDTH_0_50" }
          }
        },
        "stepConfig": { "stepType": $stepType }
      }
    """

  test("every step is taken through the custom mask"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosSouthMosObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = executionConfigQuery(oid, "gmosSouth", "science", FpuAtomQuery, 0.some),
        expected =
          json"""
            {
              "executionConfig": {
                "gmosSouth": {
                  "science": {
                    "nextAtom": {
                      "steps": [
                        ${expectedStep("GCAL")},
                        ${expectedStep("GCAL")},
                        ${expectedStep("SCIENCE")},
                        ${expectedStep("SCIENCE")},
                        ${expectedStep("SCIENCE")}
                      ]
                    },
                    "possibleFuture": [],
                    "hasMore": true
                  }
                }
              }
            }
          """.asRight
      )
