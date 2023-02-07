// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.literal.*

class cloneTarget extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi = TestUsers.Standard.pi(nextId, nextId)
  lazy val validUsers = List(pi)

  test("simple clone") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
          mutation {
            cloneTarget(input: {
              targetId: "$tid"
            }) {
              originalTarget {
                id
              }
              newTarget {
                id
              }
            }
          }
          """,
          expected = Right(
            json"""
            42
            """
          )
        )
      }
    }
  }

}