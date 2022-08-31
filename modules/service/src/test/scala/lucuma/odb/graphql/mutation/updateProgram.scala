// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import lucuma.core.math.Offset.P
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.data.Existence
import lucuma.odb.graphql.OdbSuite

class updateProgram extends OdbSuite with CreateProgramOps {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

  test("edit name") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  name: "new name"
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              id
              name
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": [
              {
                "id": $pid,
                "name": "new name"
              }
            ]
          }
          """
        )
      )
    }
  }

  test("edit existence") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  existence: DELETED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
                includeDeleted: true
              }
            ) {
              id
              existence
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": [
              {
                "id": $pid,
                "existence": ${Existence.Deleted:Existence}
              }
            ]
          }
          """
        )
      )
    }
  }

  test("edit existence") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  existence: DELETED
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
                includeDeleted: true
              }
            ) {
              id
              existence
            }
          }
        """,
        expected = Right(
          json"""
          {
            "updatePrograms": [
              {
                "id": $pid,
                "existence": ${Existence.Deleted:Existence}
              }
            ]
          }
          """
        )
      )
    }
  }

  test("edit proposal (attempt add w/ insufficient information)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              id
              proposal {
                title
              }
            }
          }
        """,
        expected =
          Left(List("One or more programs has no proposal, and there is insufficient information to create one. To add a proposal all required fields must be specified."))
      )
    }
  }

  test("edit proposal (add)") {
    createProgramAs(pi).flatMap { pid =>
      expect(
        user = pi,
        query = s"""
          mutation {
            updatePrograms(
              input: {
                SET: {
                  proposal: {
                    title: "new title"
                    proposalClass: {
                      queue: {
                        minPercentTime: 50
                      }
                    }
                    category: COSMOLOGY
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 100
                      }
                    ]
                  }
                }
                WHERE: {
                  id: {
                    EQ: "$pid"
                  }
                }
              }
            ) {
              id
              proposal {
                title
                proposalClass {
                  ... on Queue {
                    minPercentTime
                  }
                }
                category
                toOActivation
                partnerSplits {
                  partner
                  percent
                }
              }
            }
          }
        """,
        expected =
          Right(json"""
            {
              "updatePrograms" : [
                {
                  "id" : $pid,
                  "proposal" : {
                    "title" : "new title",
                    "proposalClass" : {
                      "minPercentTime" : 50
                    },
                    "category" : "COSMOLOGY",
                    "toOActivation" : "NONE",
                    "partnerSplits" : [
                      {
                        "partner" : "US",
                        "percent" : 100
                      }
                    ]
                  }
                }
              ]
            }
          """)
      )
    }
  }

}