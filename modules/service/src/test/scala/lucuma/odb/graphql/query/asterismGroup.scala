// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class asterismGroup extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  def createProgram(user: User): IO[Program.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createProgram(input: {}) {
              program {
                id
              }
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createProgram", "program", "id").require[Program.Id]
    }

  def createTarget(user: User, pid: Program.Id): IO[Target.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createTarget(input: {
              programId: ${pid.asJson}
              SET: {
                name: "my name"
                sourceProfile: {
                  point: {
                    bandNormalized:{
                      sed: {
                        coolStar: T600_K
                      }
                      brightnesses: []
                    }          
                  }
                }
                sidereal: {                  
                }
              }
            }) {
              target {
                id
              }
            }
          }
        """
    ) map { json =>
      json.hcursor.downFields("createTarget", "target", "id").require[Target.Id]
    }

  def createObservation(user: User, pid: Program.Id, tids: Target.Id*): IO[Observation.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createObservation(input: {
            programId: ${pid.asJson},
              SET: {
                targetEnvironment: {
                  asterism: ${tids.asJson}
                }
              }
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

  test("asterisms should be correctly grouped") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgram(user)
        tids <- createTarget(user, pid).replicateA(5)
        oid0 <- createObservation(user, pid, tids(3))
        oid1 <- createObservation(user, pid, tids(0), tids(1))
        oid2 <- createObservation(user, pid, tids(0), tids(1))
        oid3 <- createObservation(user, pid, tids(0), tids(1), tids(2))
        oid4 <- createObservation(user, pid)
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
                            "id" : $oid0
                          }
                        ]
                      },
                      "asterism" : [
                        {
                          "id" : ${tids(3)}
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
                          "id" : ${tids(0)}
                        },
                        {
                          "id" : ${tids(1)}
                        }
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
                          "id" : ${tids(0)}
                        },
                        {
                          "id" : ${tids(1)}
                        },
                        {
                          "id" : ${tids(2)}
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

  test("asterisms should be correctly filtered") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgram(user)
        tids <- createTarget(user, pid).replicateA(5)
        oid0 <- createObservation(user, pid, tids(3))
        oid1 <- createObservation(user, pid, tids(0), tids(1))
        oid2 <- createObservation(user, pid, tids(0), tids(1))
        oid3 <- createObservation(user, pid, tids(0), tids(1), tids(2))
        oid4 <- createObservation(user, pid)
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                asterismGroup(programId: ${pid.asJson}, WHERE: { id: { EQ: "$oid0"} }) {
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