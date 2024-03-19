// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package issue.github

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User

// https://github.com/gemini-hlsw/lucuma-odb/issues/249
class GitHub_249 extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def deleteTarget(user: User, tid: Target.Id): IO[Unit] =
    query(
      user = pi,
      query = s"""
        mutation {
          updateTargets(input: {
            SET: {
              existence: DELETED
            }
            WHERE: {
              id: { EQ: "$tid"}
            }
          }) {
            targets {
              id
            }
          }
        }
      """
    ).void

  test("asterisms should be recomputed when targets are deleted") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgramAs(user)
        tids <- createTargetAs(user, pid).replicateA(5)
        oid0 <- createObservationAs(user, pid, tids(3))
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(0), tids(1))
        oid3 <- createObservationAs(user, pid, tids(0), tids(1), tids(2))
        oid4 <- createObservationAs(user, pid)
        _    <- deleteTarget(user, tids(0))
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                asterismGroup(programId: ${pid.asJson}) {
                  matches {
                    observations {
                      matches {
                        id
                      }
                    }
                    asterism {
                      id
                    }
                  }
                }
              }
            """,
          expected = Right(
            json"""
              {
                "asterismGroup" : {
                  "matches" : [
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid4
                          }
                        ]
                      },
                      "asterism" : [
                      ]
                    },
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid3
                          }
                        ]
                      },
                      "asterism" : [
                        {
                          "id" : ${tids(1)}
                        },
                        {
                          "id" : ${tids(2)}
                        }
                      ]
                    },
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid1
                          },
                          {
                            "id" : $oid2
                          }
                        ]
                      },
                      "asterism" : [
                        {
                          "id" : ${tids(1)}
                        }
                      ]
                    },
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid0
                          }
                        ]
                      },
                      "asterism" : [
                        {
                          "id" : ${tids(3)}
                        }
                      ]
                    }
                  ]
                }
              }

            """
          )
        )
      } yield true
    }
  }

}
