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
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class targetGroup extends OdbSuite {

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

  def createTarget(user: User, pid: Program.Id, name: String = "blah"): IO[Target.Id] =
    query(
      user = user,
      query =
        s"""
          mutation {
            createTarget(input: {
              programId: "$pid"
              SET: {
                name: "$name"
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
                  ra: { degrees: 0 }       
                  dec: { degrees: 0 }       
                  epoch: "J2000.000"
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

  test("targets should be correctly grouped") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgram(user)
        tids <- createTarget(user, pid).replicateA(5)
        oid1 <- createObservation(user, pid, tids(0), tids(1))
        oid2 <- createObservation(user, pid, tids(1), tids(2), tids(3))
        oid3 <- createObservation(user, pid, tids(2))
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                targetGroup(programId: ${pid.asJson}) {
                  matches {
                    observations {
                      matches {
                        id
                      }
                    }
                    target {
                      id
                    }
                  }
                }
              }
            """,
          expected = Right(
            json"""
              {
                "targetGroup" : {
                  "matches" : [
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid1
                          }
                        ]
                      },
                      "target" : {
                        "id" : ${tids(0)}
                      }
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
                      "target" : {
                        "id" : ${tids(1)}
                      }
                    },
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid2
                          },
                          {
                            "id" : $oid3
                          }
                        ]
                      },
                      "target" : {
                        "id" : ${tids(2)}
                      }
                    },
                    {
                      "observations" : {
                        "matches" : [
                          {
                            "id" : $oid2
                          }
                        ]
                      },
                      "target" : {
                        "id" : ${tids(3)}
                      }
                    },
                    {
                      "observations" : {
                        "matches" : [
                        ]
                      },
                      "target" : {
                        "id" : ${tids(4)}
                      }
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

  test("targets group should be reflected in observation title") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgram(user)
        tids <- List("foo", "bar", "baz", "qux", "quux").traverse(createTarget(user, pid, _))
        oid1 <- createObservation(user, pid, tids(0), tids(1))
        oid2 <- createObservation(user, pid, tids(1), tids(2), tids(3))
        oid3 <- createObservation(user, pid, tids(2))
        oid4 <- createObservation(user, pid)
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                observations(programId: ${pid.asJson}) {
                  matches {
                    id
                    title
                  }
                }
              }
            """,
          expected = Right(
            json"""
              {
                "observations" : {
                  "matches" : [
                    {
                      "id" : $oid1,
                      "title" : "foo, bar"
                    },
                    {
                      "id" : $oid2,
                      "title" : "bar, baz, qux"
                    },
                    {
                      "id" : $oid3,
                      "title" : "baz"
                    },
                    {
                      "id" : $oid4,
                      "title" : "Untargeted"
                    }
                  ]
                }
              }"""
          )
        )
      } yield true
    }
  }

}