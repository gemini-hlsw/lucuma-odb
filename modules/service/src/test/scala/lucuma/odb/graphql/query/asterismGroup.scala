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
import lucuma.core.model.Semester
import lucuma.core.model.Target

class asterismGroup extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 30)
  val validUsers = List(pi)

  test("asterisms should be correctly grouped") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgramAs(user)
        tids <- createTargetAs(user, pid).replicateA(5)
        oid0 <- createObservationAs(user, pid, tids(3))
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(0), tids(1))
        oid3 <- createObservationAs(user, pid, tids(0), tids(1), tids(2))
        oid4 <- createObservationAs(user, pid)
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
        pid  <- createProgramAs(user)
        tids <- createTargetAs(user, pid).replicateA(5)
        oid0 <- createObservationAs(user, pid, tids(3))
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(0), tids(1))
        oid3 <- createObservationAs(user, pid, tids(0), tids(1), tids(2))
        oid4 <- createObservationAs(user, pid)
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

  test("lookup by proposal reference should work") {
    List(pi).traverse { user =>
      for {
        pid  <- createProgramAs(user)
        _    <- addProposal(user, pid)
        _    <- submitProposal(user, pid, Semester.unsafeFromString("2025A").some)
        tids <- createTargetAs(user, pid).replicateA(5)
        oid0 <- createObservationAs(user, pid, tids(3))
        oid1 <- createObservationAs(user, pid, tids(0), tids(1))
        oid2 <- createObservationAs(user, pid, tids(0), tids(1))
        oid3 <- createObservationAs(user, pid, tids(0), tids(1), tids(2))
        oid4 <- createObservationAs(user, pid)
        _  <- expect(
          user = user,
          query =
            s"""
              query {
                asterismGroup(proposalReference: "G-2025A-0001", WHERE: { id: { EQ: "$oid0"} }) {
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
