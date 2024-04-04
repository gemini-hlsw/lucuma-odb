// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*

class createCallForProposals extends OdbSuite {

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, staff)

  test("success - simple with defaults") {

    // status    defaults to CLOSED
    // existence defaults to PRESENT

    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2025-02-01 14:00:00"
                activeEnd:   "2025-07-31 14:00:00"
              }
            }
          ) {
            callForProposals {
              status
              type
              semester
              activeStart
              activeEnd
              existence
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "status":      "CLOSED",
              "type":        "REGULAR_SEMESTER",
              "semester":    "2025A",
              "activeStart": "2025-02-01 14:00:00",
              "activeEnd":   "2025-07-31 14:00:00",
              "existence":   "PRESENT"
            }
          }
        }
      """.asRight
    )
  }

  test("failure - end before start") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2025-07-31 14:00:00"
                activeEnd:   "2025-02-31 14:00:00"
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: activeStart must be before activeEnd").asLeft
    )
  }

  test("success - with partners") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2026-02-01 14:00:00"
                activeEnd:   "2026-07-31 14:00:00"
                partners:    [
                  {
                    partner: CA
                    deadline: "2025-07-31 10:00:00"
                  },
                  {
                    partner: US
                    deadline: "2025-07-31 10:00:01"
                  }
                ]
              }
            }
          ) {
             callForProposals {
               partners {
                 partner
                 deadline
               }
             }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "partners": [
                {
                  "partner": "CA",
                  "deadline": "2025-07-31 10:00:00"
                },
                {
                  "partner": "US",
                  "deadline": "2025-07-31 10:00:01"
                }
              ]
            }
          }
        }
      """.asRight
    )
  }

}
