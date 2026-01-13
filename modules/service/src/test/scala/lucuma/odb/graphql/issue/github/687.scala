// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Target
import lucuma.odb.graphql.query.ObservingModeSetupOperations

// https://github.com/gemini-hlsw/lucuma-odb/issues/687
class GitHub_687 extends OdbSuite with ObservingModeSetupOperations {
  val pi = TestUsers.Standard.pi(nextId, nextId)
  val validUsers = List(pi)

  test("clone target with replace_in, where obs has cached ITC results") {
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      o1  <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- expect(
        user = pi,
        query = s"""
        query {
          observation(observationId: ${o1.asJson}) {
            itc {
              ... on ItcSpectroscopy {
                spectroscopyScience {
                  index
                }
              }
            }
          }
        }
        """,
        expected = Right(
          json"""
            {
              "observation" : {
                "itc" : {
                  "spectroscopyScience" : {
                    "index" : 0
                  }
                }
              }
            }
          """
        )
      )
      _   <- expect(
        user = pi,
        query = s"""
          mutation {
            cloneTarget(input: {
              targetId: ${tid.asJson}
              REPLACE_IN: [${o1.asJson}]
            }) {
              newTarget {
                name
              }
            }
          }
        """,
        expected = Right(
          json"""
            {
              "cloneTarget" : {
                "newTarget" : {
                  "name" : "V1647 Orionis"
                }
              }
            }
          """
        )
      )
    yield ()
  }

}
