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
              id
              status
              type
              semester
              raLimitStart { hms }
              raLimitEnd { hms }
              decLimitStart { dms }
              decLimitEnd { dms }
              activeStart
              activeEnd
              partners {
                partner
              }
              instruments
              existence
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id":            "c-100",
              "status":        "CLOSED",
              "type":          "REGULAR_SEMESTER",
              "semester":      "2025A",
              "raLimitStart":  null,
              "raLimitEnd":    null,
              "decLimitStart": null,
              "decLimitEnd":   null,
              "activeStart":   "2025-02-01 14:00:00",
              "activeEnd":     "2025-07-31 14:00:00",
              "partners":      [],
              "instruments":   [],
              "existence":     "PRESENT"
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
               id
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
              "id": "c-101",
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

  test("success - with instruments") {
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
                instruments: [GMOS_SOUTH, GMOS_NORTH]
              }
            }
          ) {
             callForProposals {
               id
               instruments
             }
          }
        }
      """,
      // sorted by instrument name
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id": "c-102",
              "instruments": [ "GMOS_NORTH", "GMOS_SOUTH" ]
            }
          }
        }
      """.asRight
    )
  }
}
