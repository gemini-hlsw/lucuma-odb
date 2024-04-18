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

  test("failure - only staff may create calls") {
    expect(
      user = pi,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2025-02-31 14:00:00"
                activeEnd:   "2025-07-31 14:00:00"
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("User u-1 is not authorized to perform this operation.").asLeft
    )
  }

  test("success - simple with defaults") {

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
              type
              semester
              raLimitStart { hms }
              raLimitEnd { hms }
              decLimitStart { dms }
              decLimitEnd { dms }
              active {
                start
                end
                duration { hours }
              }
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
              "type":          "REGULAR_SEMESTER",
              "semester":      "2025A",
              "raLimitStart":  null,
              "raLimitEnd":    null,
              "decLimitStart": null,
              "decLimitEnd":   null,
              "active": {
                "start": "2025-02-01 14:00:00",
                "end": "2025-07-31 14:00:00",
                "duration": { "hours": 4320.000000 }
              },
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
                    submissionDeadline: "2025-07-31 10:00:00"
                  },
                  {
                    partner: US
                    submissionDeadline: "2025-07-31 10:00:01"
                  }
                ]
              }
            }
          ) {
             callForProposals {
               id
               partners {
                 partner
                 submissionDeadline
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
                  "submissionDeadline": "2025-07-31 10:00:00"
                },
                {
                  "partner": "US",
                  "submissionDeadline": "2025-07-31 10:00:01"
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

  test("success - with ra") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:         REGULAR_SEMESTER
                semester:     "2025A"
                activeStart:  "2026-02-01 14:00:00"
                activeEnd:    "2026-07-31 14:00:00"
                raLimitStart: { hms: "12:00:00" }
                raLimitEnd:   { hms: "18:00:00" }
              }
            }
          ) {
             callForProposals {
               id
               raLimitStart { hms }
               raLimitEnd { hms }
             }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id": "c-103",
              "raLimitStart": { "hms": "12:00:00.000000" },
              "raLimitEnd":   { "hms": "18:00:00.000000" }
            }
          }
        }
      """.asRight
    )
  }

  test("failure - missing ra end") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:         REGULAR_SEMESTER
                semester:     "2025A"
                activeStart:  "2025-02-31 14:00:00"
                activeEnd:    "2025-07-31 14:00:00"
                raLimitStart: { hms: "12:00:00" }
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: Supply both raLimitStart and raLimitEnd or neither").asLeft
    )
  }

  test("success - with dec") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:         REGULAR_SEMESTER
                semester:     "2025A"
                activeStart:  "2026-02-01 14:00:00"
                activeEnd:    "2026-07-31 14:00:00"
                decLimitStart: { dms: "45:00:00" }
                decLimitEnd:   { dms: "-45:00:00" }
              }
            }
          ) {
             callForProposals {
               id
               decLimitStart { dms }
               decLimitEnd { dms }
             }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id": "c-104",
              "decLimitStart": { "dms": "+45:00:00.000000" },
              "decLimitEnd":   { "dms": "-45:00:00.000000" }
            }
          }
        }
      """.asRight
    )
  }

  test("failure - missing dec start") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2025-02-31 14:00:00"
                activeEnd:   "2025-07-31 14:00:00"
                decLimitEnd: { dms: "12:00:00" }
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: Supply both decLimitStart and decLimitEnd or neither").asLeft
    )
  }

}
