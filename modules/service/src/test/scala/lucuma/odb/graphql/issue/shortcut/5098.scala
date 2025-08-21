// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import cats.data.Ior
import io.circe.literal.*
import lucuma.core.model.Target
import lucuma.odb.graphql.query.ExecutionTestSupportForGmos

class ShortCut_5098 extends ExecutionTestSupportForGmos:

  def removeSED(t: Target.Id) = query(pi,
    s"""
      mutation {
        updateTargets(input: {
          SET: {
            sourceProfile: {
              point: {
                bandNormalized: {
                  sed: null
                }
              }
            }
          }
          WHERE: {
            id: { EQ: "$t"}
          }
        }) {
          targets {
            id
          }
        }
      }
    """)

  test("Detect if target has no SED before calling ITC"):
    val setup =
      for
        p <- createProgram
        t <- createTargetWithProfileAs(pi, p)
        _ <- removeSED(t)
        o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
        _ <- runObscalcUpdate(p, o)
      yield (t, o)
    setup.flatMap { case (tid, oid) =>
      expectIor(
        user  = pi,
        query =
          s"""
             query {
               observation(observationId: "$oid") {
                 workflow {
                   value {
                     validationErrors {
                       messages
                     }
                   }
                 }
                 execution {
                   digest {
                     value {
                       science {
                         timeEstimate {
                           total {
                             seconds
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           """,
        expected = Ior.both(
          List(s"ITC cannot be queried until the following parameters are defined: SED"),
          json"""
            {
              "observation": {
                "workflow": {
                  "value": {
                    "validationErrors": [
                      {
                        "messages": [ "Missing SED" ]
                      }
                    ]
                  }
                },
                "execution": {
                  "digest": {
                    "value": null
                  }
                }
              }
            }
          """
        )
      )
    }
