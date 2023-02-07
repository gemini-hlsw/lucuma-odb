// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Target

class cloneTarget extends OdbSuite {
  import createTarget.FullTargetGraph

  val pi, pi2 = TestUsers.Standard.pi(nextId, nextId)

  lazy val validUsers = List(pi, pi2)

  test("simple clone") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        query(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
              }) {
                originalTarget $FullTargetGraph
                newTarget $FullTargetGraph

                originalTargetId: originalTarget { id }
                newTargetId: newTarget { id }
              }
            }
          """
        ).map { json =>

          // The data fields (i.e., everything but ID) should be the same
          assertEquals(
            json.hcursor.downFields("cloneTarget", "originalTarget").as[Json],
            json.hcursor.downFields("cloneTarget", "newTarget").as[Json]
          )

          // The ids should be different
          assertNotEquals(
            json.hcursor.downFields("cloneTarget", "originalTargetId", "Id").as[Target.Id],
            json.hcursor.downFields("cloneTarget", "newTargetId", "Id").as[Target.Id]
          )
        
        }
      }
    }
  }

  test("clone with rename") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
                SET: {
                  name: "New Name"
                }
              }) {
                originalTarget {
                  name
                }
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
                  "originalTarget" : {
                    "name" : "My Target"
                  },
                  "newTarget" : {
                    "name" : "New Name"
                  }
                }
              }
            """
          )
        )
      }
    }
  }

  test("clone with bogus target id") {
    expect(
      user = pi,
      query = s"""
        mutation {
          cloneTarget(input: {
            targetId: "t-ffff"
          }) {
            newTarget {
              id
            }
          }
        }
      """,
      expected = Left(List("No such target: t-ffff"))
    )
  }                

  test("clone with bogus update") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi,
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
                SET: {
                  sourceProfile: {
                    gaussian: {
                    }
                  }
                }
              }) {
                newTarget {
                  id
                }
              }
            }
          """,
          expected = Left(List("Not a gaussian source.  To change profile type, please provide a full definition."))
        )
      }
    }
  }

  test("clone someone else's target") {
    createProgramAs(pi).flatMap { pid =>
      createTargetAs(pi, pid, "My Target").flatMap { tid =>
        expect(
          user = pi2, // different user!
          query = s"""
            mutation {
              cloneTarget(input: {
                targetId: "$tid"
              }) {
                newTarget {
                  id
                }
              }
            }
          """,
          expected = Left(List(s"No such target: $tid"))
        )
      }
    }
  }

}