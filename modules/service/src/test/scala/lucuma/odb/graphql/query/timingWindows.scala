// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User

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

  def updateObservation(user: User,  oid: Observation.Id, twisOpt: Option[String]): IO[Json] =
    query(
      user = user,
      query =
        s"""
          mutation {
            updateObservations(input: {
              WHERE: {
                id: {
                  EQ: "$oid"
                }
              },
              SET: {
        """ + 
        twisOpt.map(twis => 
          s"timingWindows: $twis"
        ).orEmpty +
        s"""
              }
            }) {
              observations {
                $TimingWindowsGraph
              }
            }
          }
        """

    ).map { json => 
      json.hcursor.downFields("updateObservations").require[Json]
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

  val TimingWindowsInput2 = 
   """[
        {
          inclusion: EXCLUDE,
          startUtc: "2023-04-01 00:00:00",
          end: {
            after: {
              hours: 48
            }
          }
        },
        {
          inclusion: INCLUDE,
          startUtc: "2023-04-04 00:00:00",
          end: {
            atUtc: "2023-04-08 00:00:00"
          }
        }
      ]"""

  val TimingWindowsOutput2 = 
    json"""
      {
        "observations" : [
          {
            "timingWindows" : [
              {
                "inclusion": "EXCLUDE",
                "startUtc": "2023-04-01 00:00:00",
                "end": {
                  "after": {
                    "hours": 48.000000
                  },
                  "repeat": null
                }
              },
              {
                "inclusion": "INCLUDE",
                "startUtc": "2023-04-04 00:00:00",
                "end": {
                  "atUtc": "2023-04-08 00:00:00"
                }
              }
            ]
          }
        ]
      }
    """

  test("timing windows edit to new list") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        createObservation(user, pid, TimingWindowsInput.some).flatMap{ obsId =>
          updateObservation(user, obsId, TimingWindowsInput2.some).assertEquals(TimingWindowsOutput2)
        }
      }
    }
  }

  test("timing windows edit to null") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        createObservation(user, pid, TimingWindowsInput.some).flatMap{ obsId =>
          updateObservation(user, obsId, "null".some).assertEquals(json"""{ "observations": [{ "timingWindows": [] }]}""")
        }
      }
    }
  }

  test("timing windows edit omit value") {
    List(pi).traverse { user =>
      createProgramAs(user).flatMap { pid =>
        createObservation(user, pid, TimingWindowsInput2.some).flatMap{ obsId =>
          updateObservation(user, obsId, none).assertEquals(TimingWindowsOutput2)
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
                observations(WHERE: {
                  program: {
                    id: { EQ: "$pid" }
                  }
                }) {
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
