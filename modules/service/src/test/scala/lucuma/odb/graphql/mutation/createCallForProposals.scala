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
                semester:    "2024B"
                activeStart: "2024-07-31 14:00:00"
                activeEnd:   "2025-02-01 14:00:00"
              }
            }
          ) {
            callForProposals {
              id
              type
              semester
              coordinateLimits {
                north {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
                south {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
              }
              active {
                start
                end
                duration { hours }
              }
              submissionDeadlineDefault
              partners {
                partner
                submissionDeadline
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
              "semester":      "2024B",
              "coordinateLimits": {
                "north": {
                  "raStart": { "hms": "16:30:00.000000" },
                  "raEnd": { "hms": "14:00:00.000000" },
                  "decStart": { "dms": "+00:00:00.000000" },
                  "decEnd": { "dms": "+00:00:00.000000" }
                },
                "south": {
                  "raStart": { "hms": "15:30:00.000000" },
                  "raEnd": { "hms": "12:30:00.000000" },
                  "decStart": { "dms": "+00:00:00.000000" },
                  "decEnd": { "dms": "+00:00:00.000000" }
                }
              },
              "active": {
                "start": "2024-07-31 14:00:00",
                "end": "2025-02-01 14:00:00",
                "duration": { "hours": 4440.000000 }
              },
              "submissionDeadlineDefault": null,
              "partners": [
                {
                  "partner" : "AR",
                  "submissionDeadline": null
                },
                {
                  "partner" : "BR",
                  "submissionDeadline": null
                },
                {
                  "partner" : "CA",
                  "submissionDeadline": null
                },
                {
                  "partner" : "CL",
                  "submissionDeadline": null
                },
                {
                  "partner" : "KR",
                  "submissionDeadline": null
                },
                {
                  "partner" : "UH",
                  "submissionDeadline": null
                },
                {
                  "partner" : "US",
                  "submissionDeadline": null
                }
              ],
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
      expected = List("Argument 'input.SET' is invalid: activeStart must come before activeEnd").asLeft
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
                submissionDeadlineDefault: "2025-07-31 10:00:02"
                partners:    [
                  {
                    partner: CA
                    submissionDeadlineOverride: "2025-07-31 10:00:00"
                  },
                  {
                    partner: CL
                  },
                  {
                    partner: US
                    submissionDeadlineOverride: "2025-07-31 10:00:01"
                  }
                ]
              }
            }
          ) {
             callForProposals {
               id
               submissionDeadlineDefault
               partners {
                 partner
                 submissionDeadlineOverride
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
              "submissionDeadlineDefault": "2025-07-31 10:00:02",
              "partners": [
                {
                  "partner": "CA",
                  "submissionDeadlineOverride": "2025-07-31 10:00:00",
                  "submissionDeadline": "2025-07-31 10:00:00"
                },
                {
                  "partner": "CL",
                  "submissionDeadlineOverride": null,
                  "submissionDeadline": "2025-07-31 10:00:02"
                },
                {
                  "partner": "US",
                  "submissionDeadlineOverride": "2025-07-31 10:00:01",
                  "submissionDeadline": "2025-07-31 10:00:01"
                }
              ]
            }
          }
        }
      """.asRight
    )
  }

  test("success - with empty partners") {
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
                partners:    []
              }
            }
          ) {
             callForProposals {
               id
               partners { partner }
             }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id": "c-102",
              "partners": []
            }
          }
        }
      """.asRight
    )
  }

  test("failure - with duplicate partners") {
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
                    partner: US
                    submissionDeadlineOverride: "2025-07-31 10:00:00"
                  },
                  {
                    partner: US
                    submissionDeadlineOverride: "2025-07-31 10:00:01"
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
      expected = List("Argument 'input.SET' is invalid: duplicate 'partners' specified: US").asLeft
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
              "instruments": [ "GMOS_NORTH", "GMOS_SOUTH" ]
            }
          }
        }
      """.asRight
    )
  }

  test("failure - with duplicate instruments") {
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
                instruments: [GMOS_SOUTH, GMOS_SOUTH]
              }
            }
          ) {
             callForProposals {
               instruments
             }
          }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: duplicate 'instruments' specified: GMOS_SOUTH").asLeft
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
                semester:    "2024B"
                activeStart: "2024-07-31 14:00:00"
                activeEnd:   "2025-02-01 14:00:00"
                coordinateLimits: {
                  north: {
                    raStart: { hms: "17:00:00" }
                    raEnd: { hms: "13:00:00" }
                  }
                }
              }
            }
          ) {
            callForProposals {
              coordinateLimits {
                north {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
                south {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "coordinateLimits": {
                "north": {
                  "raStart": { "hms": "17:00:00.000000" },
                  "raEnd": { "hms": "13:00:00.000000" },
                  "decStart": { "dms": "+00:00:00.000000" },
                  "decEnd": { "dms": "+00:00:00.000000" }
                },
                "south": {
                  "raStart": { "hms": "15:30:00.000000" },
                  "raEnd": { "hms": "12:30:00.000000" },
                  "decStart": { "dms": "+00:00:00.000000" },
                  "decEnd": { "dms": "+00:00:00.000000" }
                }
              }
            }
          }
        }
      """.asRight
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
                semester:    "2024B"
                activeStart: "2024-07-31 14:00:00"
                activeEnd:   "2025-02-01 14:00:00"
                coordinateLimits: {
                  south: {
                    decStart: { dms: "45:00:00" }
                    decEnd: { dms: "-45:00:00" }
                  }
                }
              }
            }
          ) {
             callForProposals {
              coordinateLimits {
                north {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
                south {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "coordinateLimits": {
                "north": {
                  "raStart": { "hms": "16:30:00.000000" },
                  "raEnd": { "hms": "14:00:00.000000" },
                  "decStart": { "dms": "+00:00:00.000000" },
                  "decEnd": { "dms": "+00:00:00.000000" }
                },
                "south": {
                  "raStart": { "hms": "15:30:00.000000" },
                  "raEnd": { "hms": "12:30:00.000000" },
                  "decStart": { "dms": "+45:00:00.000000" },
                  "decEnd": { "dms": "-45:00:00.000000" }
                }
              }
            }
          }
        }
      """.asRight
    )
  }

}
