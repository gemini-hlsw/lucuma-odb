// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

class sequence extends OdbSuite with ObservingModeSetupOperations {

  val user: User = TestUsers.service(3)

  override val validUsers: List[User] =
    List(user)

  val createProgram: IO[Program.Id] =
    createProgramAs(user, "Sequence Testing")

  def setup: IO[(Program.Id, Observation.Id, Target.Id)] =
    for {
      p <- createProgram
      t <- createTargetWithProfileAs(user, p)
      o <- createGmosNorthLongslitObservationAs(user, p, t)
    } yield (p, o, t)

  test("simple generation") {
    setup.flatMap { case (pid, oid, tid) =>
      expect(
        user  = user,
        query =
          s"""
             query {
               sequence(programId: "$pid", observationId: "$oid") {
                 programId
                 executionConfig {
                   ... on GmosNorthExecutionConfig {
                     static {
                       stageMode
                       detector
                       mosPreImaging
                       nodAndShuffle {
                         posA { p { microarcseconds } }
                       }
                     }
                     acquisition {
                       nextAtom {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                             readout {
                               xBin
                               yBin
                             }
                             roi,
                             fpu {
                               builtin
                             }
                           }
                         }
                       }
                       possibleFuture {
                         steps {
                           instrumentConfig {
                             exposure {
                               seconds
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Right(
          json"""
            {
              "sequence": {
                "programId": $pid,
                "executionConfig": {
                  "static": {
                    "stageMode": "FOLLOW_XY",
                    "detector": "HAMAMATSU",
                    "mosPreImaging": "IS_NOT_MOS_PRE_IMAGING",
                    "nodAndShuffle": null
                  },
                  "acquisition": {
                    "nextAtom": {
                      "steps": [
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 10.000000
                            },
                            "readout": {
                              "xBin": "TWO",
                              "yBin": "TWO"
                            },
                            "roi": "CCD2",
                            "fpu": null
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 20.000000
                            },
                            "readout": {
                              "xBin": "ONE",
                              "yBin": "ONE"
                            },
                            "roi": "CENTRAL_STAMP",
                            "fpu": {
                              "builtin": "LONG_SLIT_0_50"
                            }
                          }
                        },
                        {
                          "instrumentConfig": {
                            "exposure": {
                              "seconds": 40.000000
                            },
                            "readout": {
                              "xBin": "ONE",
                              "yBin": "ONE"
                            },
                            "roi": "CENTRAL_STAMP",
                            "fpu": {
                              "builtin": "LONG_SLIT_0_50"
                            }
                          }
                        }
                      ]
                    },
                    "possibleFuture": []
                  }
                }
              }
            }
          """
        )
      )
    }

  }
}
