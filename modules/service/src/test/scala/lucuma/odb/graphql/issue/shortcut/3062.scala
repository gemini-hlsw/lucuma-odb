// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.shortcut

import io.circe.literal.*
import lucuma.core.model.User

class ShortCut_3062 extends OdbSuite {
  val pi: User    = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(pi)

  test("Deleted targets should be excluded from observation title when asterism is updated") {
    for {
      pid  <- createProgramAs(pi)
      tid0 <- createTargetAs(pi, pid, "Zero")
      oid  <- createObservationAs(pi, pid, None, tid0)
      tid1 <- createTargetAs(pi, pid, "One") // One added
      _    <- deleteTargetAs(pi, tid0) // Zero removed
      _    <- updateAsterisms(pi, List(oid), List(tid1), Nil, List((oid, List(tid1))))
      _    <- expect(
                user = pi,
                query =
                  s"""
                    query {
                      observation(observationId: "$oid") {
                        title
                        targetEnvironment {
                          asterism {
                            name
                          }
                        }
                      }
                    }
                  """,
                expected = Right(
                  json"""
                    {
                      "observation" : {
                        "title" : "One",
                        "targetEnvironment" : {
                          "asterism" : [{
                            "name" : "One"
                          }]
                        }
                      }
                    }
                  """
                )
              )
    } yield ()
  }

}
