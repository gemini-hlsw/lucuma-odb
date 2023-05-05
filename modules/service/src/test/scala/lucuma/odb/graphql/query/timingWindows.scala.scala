// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.literal._
import io.circe.syntax._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class timingWindows extends OdbSuite {
  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def createObservation(user: User, pid: Program.Id, twisOpt: Option[String]): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
              programId: ${pid.asJson}
        """ + 
        twisOpt.map(twis => 
          s""",
              SET: {
                timingWindows: $twis
              }
          """
        ).orEmpty +
        """
            }) {
              observation {
                id
              }
            }
          }
        """
    ).map { json => 
      json.hcursor.downFields("createObservation", "observation", "id").require[Observation.Id]
    }

  test("null timing windows should result in an empty set") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        createObservation(user, pid, none).flatMap{ obsId =>
          expect(
            user = user,
            query = 
              s"""
                query {
                  observation(observationId: "$obsId") {
                    $TimingWindowsGraph
                  }
                }
              """,
            expected = Right(
              json"""
                {
                  "observation" : {
                    "timingWindows" : [
                    ]
                  }
                }
              """
            )
          )
        }
      }
    }
  }

  test("empty timing windows should work") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        createObservation(user, pid, "[]".some).flatMap{ obsId =>
          expect(
            user = user,
            query = 
              s"""
                query {
                  observation(observationId: "$obsId") {
                    $TimingWindowsGraph
                  }
                }
              """, 
            expected = Right(
              json"""
                {
                  "observation" : {
                    "timingWindows" : [
                    ]
                  }
                }
              """
            )
          )
        }
      }
    }
  }

  val TimingWindowsInput = 
   """[
        {
          inclusion: INCLUDE,
          startUtc: "2023-04-01 00:00:00"
        },
        {
          inclusion: INCLUDE,
          startUtc: "2023-05-01 00:00:00",
          end: {
            atUtc: "2023-05-02 00:00:00"
          }
        },
        {
          inclusion: EXCLUDE,
          startUtc: "2023-04-15 00:00:00",
          end: {
            after: {
              hours: 2
            },
            repeat: {
              period: {
                hours: 4
              },
              times: 50
            }
          }
        }
      ]"""

  val TimingWindowsOutput = 
    json"""
      {
        "observation" : {
          "timingWindows" : [
            {
              "inclusion": "INCLUDE",
              "startUtc": "2023-04-01 00:00:00",
              "end": null
            },
            {
              "inclusion": "INCLUDE",
              "startUtc": "2023-05-01 00:00:00",
              "end": {
                "atUtc": "2023-05-02 00:00:00"
              }
            },
            {
              "inclusion": "EXCLUDE",
              "startUtc": "2023-04-15 00:00:00",
              "end": {
                "after": {
                  "hours": 2.000000
                },
                "repeat": {
                  "period": {
                    "hours": 4.000000
                  },
                  "times": 50
                }
              }
            }
          ]
        }
      }
    """

  test("timing windows output should match input, preserving creation order") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        createObservation(user, pid, TimingWindowsInput.some).flatMap{ obsId =>
          expect(
            user = user,
            query = 
              s"""
                query {
                  observation(observationId: "$obsId") {
                    $TimingWindowsGraph
                  }
                }
              """,
            expected = Right(TimingWindowsOutput)
          )
        }
      }
    }
  }

  // This test addresses the case where type information was lost in the joins,
  // and is now solved via `.withDomain` in the skunk codec.
  test("we should be able to query constraints and timing windows") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        expect(
            user = user,
            query =
              s"""
              query {
                observations(programId: ${pid.asJson}) {
                  matches {
                    constraintSet {
                      elevationRange {
                        airMass {
                          min
                        }
                      }
                    }
                    timingWindows {
                      end {
                        ... on TimingWindowEndAfter {
                          after {
                            milliseconds
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
                  "observations" : {
                    "matches" : [
                    ]
                  }
                }
              """
            )
        )
      }
    }
  }
}
