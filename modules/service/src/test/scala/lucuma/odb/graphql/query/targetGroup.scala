// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.core.enums.CallForProposalsType.DemoScience
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Semester
import lucuma.core.model.Target

class targetGroup extends OdbSuite {

  val pi         = TestUsers.Standard.pi(nextId, nextId)
  val staff      = TestUsers.Standard.staff(nextId, nextId)
  val validUsers = List(pi, staff)

  test("targets should be correctly grouped") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgramAs(user)
        tids <- createTargetAs(user, pid).replicateA(5)
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(1), tids(2), tids(3))
        oid3 <- createObservationAs(user, pid, tids(2))
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
        pid  <- createProgramAs(user)
        tids <- List("foo", "bar", "baz", "qux", "quux").traverse(createTargetAs(user, pid, _))
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(1), tids(2), tids(3))
        oid3 <- createObservationAs(user, pid, tids(2))
        oid4 <- createObservationAs(user, pid)
        _  <- expect(
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

  test("should work with a proposal reference") {
    List(pi).traverse { user =>
      for {
        cid  <- createCallForProposalsAs(staff, DemoScience, Semester.unsafeFromString("2025A"))
        pid  <- createProgramWithUsPi(user)
        _    <- addDemoScienceProposal(user, pid, cid)
        _    <- submitProposal(user, pid)
        tids <- createTargetAs(user, pid).replicateA(3)
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(1), tids(2))
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                targetGroup(proposalReference: "G-2025A-0001") {
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
                          }
                        ]
                      },
                      "target" : {
                        "id" : ${tids(2)}
                      }
                    }
                  ]
                }
              }
            """
          )
        )
      } yield ()
    }
  }

  test("should handle missing program id and reference") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgramAs(user)
        tids <- createTargetAs(user, pid).replicateA(2)
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                targetGroup {
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
                  ]
                }
              }
            """
          )
        )
      } yield ()
    }
  }
}
