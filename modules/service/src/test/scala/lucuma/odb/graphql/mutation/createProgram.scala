// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.all._
import io.circe.literal._
import lucuma.core.model.Partner
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.odb.graphql.OdbSuite

class createProgram extends OdbSuite {

  val pi       = TestUsers.Standard.pi(1, 101)
  val ngo      = TestUsers.Standard.ngo(2, 102, Partner.Ca)
  val staff    = TestUsers.Standard.staff(3, 103)
  val admin    = TestUsers.Standard.admin(4, 104)
  val guest    = TestUsers.guest(5)
  val service  = TestUsers.service(6)

  val validUsers = List(pi, ngo, staff, admin, guest, service).toList

  test("empty 'name' is disallowed") {
    expect(
      user = pi,
      query =
        """
          mutation {
            createProgram(
              input: {
                SET: {
                  name: ""
                }
              }
            ) {
              program {
                id
              }
            }
          }
        """,
      expected =
        Left(
          List(
            "Argument 'input.SET.name' is invalid: string value must be non-empty."
          )
        ),
    )
  }

  test("null 'name' is ok") {
    expect(
      user = pi,
      query =
        s"""
          mutation {
            createProgram(
              input: {
                SET: {
                  name: null
                }
              }
            ) {
              program {
                name
              }
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram": {
              "program": {
                "name": null
              }
            }
          }
        """
      )
    )
  }

  test("guest + standard/pi,ngo,staff,admin user becomes the PI") {
    List(guest, pi, ngo, staff, admin).traverse { u =>
      val name = s"${u.displayName}'s Science Program"
      expect(
        user   = u,
        query  =
          s"""
            mutation {
              createProgram(
                input: {
                  SET: {
                    name: "$name"
                  }
                }
              ) {
                program {
                  name
                  pi {
                    id
                  }
                }
              }
            }
          """,
        expected = Right(
          json"""
            {
              "createProgram" : {
                "program": {
                  "name" : $name,
                  "pi" : {
                      "id" : ${u.id}
                  }
                }
              }
            }
          """
        ),
      )
    }
  }

  test("service user does not become the PI") {
    val name = s"${service.displayName}'s Science Program"
    expect(
      user   = service,
      query  =
        s"""
          mutation {
            createProgram(
              input: {
                SET: {
                  name: "$name"
                }
              }
            ) {
              program {
                name
                pi {
                  id
                }
              }
            }
          }
        """,
      expected = Right(
        json"""
          {
            "createProgram" : {
              "program": {
                "name" : $name,
                "pi" : null
              }
            }
          }
        """
      ),
    )
  }

  test("proposal properties persist") {
    expect(
      user = pi,
      query =
        """
          mutation {
            createProgram(
              input: {
                SET: {
                  proposal: {
                    title: "My Proposal"
                    proposalClass: {
                      intensive: {
                        minPercentTime: 40
                        minPercentTotalTime: 20
                        totalTime: {
                          hours: 1.23
                        }
                      }
                    }
                    toOActivation: NONE
                    partnerSplits: [
                      {
                        partner: US
                        percent: 70
                      },
                      {
                        partner: CA
                        percent: 30
                      }
                    ]
                  }
                }
              }
            ) {
              program {
                proposal {
                  title
                  proposalClass {
                    ... on Intensive {
                      minPercentTime
                      minPercentTotalTime
                      totalTime {
                        hours
                        iso
                      }
                    }
                    ... on LargeProgram {
                      LPminPercentTime: minPercentTime
                      LPminPercentTotalTime: minPercentTotalTime
                      LPtotalTime: totalTime {
                        hours
                        iso
                      }
                    }
                    ... on Queue {
                      QminPercentTime: minPercentTime
                    }
                    ... on Classical {
                      CminPercentTime: minPercentTime
                    }
                  }
                  toOActivation
                  partnerSplits {
                    partner
                    percent
                  }
                }
              }
            }
          }
        """,
      expected =
        Right(
          json"""
            {
              "createProgram" : {
                "program" : {
                  "proposal" : {
                    "title" : "My Proposal",
                    "proposalClass" : {
                      "minPercentTime" : 40,
                      "minPercentTotalTime" : 20,
                      "totalTime" : {
                        "hours" : 1.230000,
                        "iso" : "PT1H13M48S"
                      }
                    },
                    "toOActivation" : "NONE",
                    "partnerSplits" : [
                      {
                        "partner" : "CA",
                        "percent" : 30
                      },
                      {
                        "partner" : "US",
                        "percent" : 70
                      }
                    ]
                  }
                }
              }
            }
        """
        ),
    )
  }

}
