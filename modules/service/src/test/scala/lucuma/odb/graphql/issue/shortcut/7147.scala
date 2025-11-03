// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.query

import cats.effect.IO
import cats.syntax.either.*
import eu.timepit.refined.types.numeric.PosInt
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.model.Observation
import lucuma.core.syntax.string.*
import lucuma.core.syntax.timespan.*
import lucuma.core.util.TimeSpan
import lucuma.itc.IntegrationTime

class ShortCut_7147 extends ExecutionTestSupportForFlamingos2:
  val ExposureTime: TimeSpan = 20.secondTimeSpan

  override def fakeItcSpectroscopyResult: IntegrationTime =
    IntegrationTime(ExposureTime, PosInt.unsafeFrom(4))

  test("override the read mode"):
    val setup: IO[Observation.Id] =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        o <- createFlamingos2LongSlitObservationAs(pi, p, List(t))
        _ <- query(
          user  = pi,
          query = s"""
            mutation {
              updateObservations(input: {
                SET: {
                  observingMode: {
                    flamingos2LongSlit: {
                      explicitReadMode: ${Flamingos2ReadMode.Bright.tag.toScreamingSnakeCase}
                    }
                  }
                },
                WHERE: {
                  id: { EQ: "$o" }
                }
              }) {
                observations {
                  id
                }
              }
            }
          """
        )
      yield o

    setup.flatMap: oid =>
      expect(
        user     = pi,
        query    = s"""
          query {
            executionConfig(observationId: "$oid", futureLimit: 1) {
              flamingos2 {
                science {
                  nextAtom {
                    steps {
                      instrumentConfig {
                        readMode
                      }
                    }
                  }
                }
              }
            }
          }
        """,
        expected =
          json"""
            {
              "executionConfig": {
                "flamingos2": {
                  "science": {
                    "nextAtom": {
                      "steps": [
                        {
                          "instrumentConfig": {
                            "readMode": "BRIGHT"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "readMode": "BRIGHT"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "readMode": "BRIGHT"
                          }
                        },
                        {
                          "instrumentConfig": {
                            "readMode": "BRIGHT"
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
