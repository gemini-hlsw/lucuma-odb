// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Observation
import lucuma.core.syntax.timespan.*
import lucuma.itc.IntegrationTime

class executionAcqGmosNorth_slow extends ExecutionTestSupportForGmos with mutation.UpdateObservationsOps:

  override def fakeItcImagingResult: IntegrationTime =
    IntegrationTime(
      61.secTimeSpan,
      PosInt.unsafeFrom(1)
    )

  val ReadoutQuery: String =
    s"""
      steps {
        instrumentConfig {
          exposure { seconds }
          readout {
            ampReadMode
            ampGain
          }
        }
      }
    """

  test("initial generation"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid") {
              gmosNorth {
                acquisition {
                  nextAtom {
                    $ReadoutQuery
                  }
                }
              }
            }
          }
        """,
        expected = json"""
          {
            "executionConfig": {
              "gmosNorth": {
                "acquisition": {
                  "nextAtom": {
                    "steps": [
                      {
                        "instrumentConfig": {
                          "exposure": { "seconds": 61.000000 },
                          "readout": {
                            "ampReadMode": "SLOW",
                            "ampGain": "LOW"
                          }
                        }
                      },
                      {
                        "instrumentConfig": {
                          "exposure": { "seconds": 20.000000 },
                          "readout": {
                            "ampReadMode": "FAST",
                            "ampGain": "LOW"
                          }
                        }
                      },
                      {
                        "instrumentConfig": {
                          "exposure": { "seconds": 183.000000 },
                          "readout": {
                            "ampReadMode": "SLOW",
                            "ampGain": "LOW"
                          }
                        }
                      }
                    ]
                  }
                }
              }
            }
          }
        """.asRight
      )