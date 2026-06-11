// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.either.*
import io.circe.literal.*

class createCallForProposals extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 101)
  val staff = TestUsers.Standard.staff(3, 103)

  val validUsers = List(pi, staff)

  test("failure - only staff may create calls"):
    expect(
      user = pi,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2025-02-28"
                activeEnd:   "2025-07-31"
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("User u-1 is not authorized to perform this operation.").asLeft
    )

  test("failure - start too far in the future for LST calc"):
    expect(
      user = pi,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2100-02-28"
                activeEnd:   "2025-07-31"
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: 'activeStart' date (2100-02-28) must be between 1900 and 2100 UTC (exclusive)").asLeft
    )

  test("failure - end too far in the future for LST calc"):
    expect(
      user = pi,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2025-02-28"
                activeEnd:   "2100-07-31"
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: 'activeEnd' date (2100-07-31) must be between 1900 and 2100 UTC (exclusive)").asLeft
    )

  test("success - simple with defaults"):

    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2024B"
                activeStart: "2024-07-31"
                activeEnd:   "2025-02-01"
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) {
            callForProposals {
              id
              semester
              active {
                start
                end
              }
              submissionDeadlineDefault
              partners {
                geminiPartner
                submissionDeadline
              }
              existence
              gemini {
                type
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
                instruments
                proprietaryMonths
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id":       "c-100",
              "semester": "2024B",
              "active": {
                "start": "2024-07-31",
                "end": "2025-02-01"
              },
              "submissionDeadlineDefault": null,
              "partners": [
                {
                  "geminiPartner" : "AR",
                  "submissionDeadline": null
                },
                {
                  "geminiPartner" : "BR",
                  "submissionDeadline": null
                },
                {
                  "geminiPartner" : "CA",
                  "submissionDeadline": null
                },
                {
                  "geminiPartner" : "CL",
                  "submissionDeadline": null
                },
                {
                  "geminiPartner" : "KR",
                  "submissionDeadline": null
                },
                {
                  "geminiPartner" : "UH",
                  "submissionDeadline": null
                },
                {
                  "geminiPartner" : "US",
                  "submissionDeadline": null
                }
              ],
              "existence":         "PRESENT",
              "gemini": {
                "type": "REGULAR_SEMESTER",
                "coordinateLimits": {
                  "north": {
                    "raStart": { "hms": "16:30:00.000000" },
                    "raEnd": { "hms": "14:00:00.000000" },
                    "decStart": { "dms": "-37:00:00.000000" },
                    "decEnd": { "dms": "+90:00:00.000000" }
                  },
                  "south": {
                    "raStart": { "hms": "15:30:00.000000" },
                    "raEnd": { "hms": "12:30:00.000000" },
                    "decStart": { "dms": "-90:00:00.000000" },
                    "decEnd": { "dms": "+28:00:00.000000" }
                  }
                },
                "instruments":       [],
                "proprietaryMonths": 12
              }
            }
          }
        }
      """.asRight
    )

  test("failure - end before start"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2025-07-31"
                activeEnd:   "2025-02-28"
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) { callForProposals { id } }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: 'activeStart' must come before 'activeEnd'").asLeft
    )

  test("success - with partners"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2026-02-01"
                activeEnd:   "2026-07-31"
                submissionDeadlineDefault: "2025-07-31T10:00:02Z"
                partners:    [
                  {
                    geminiPartner: CA
                    submissionDeadlineOverride: "2025-07-31T10:00:00Z"
                  },
                  {
                    geminiPartner: CL
                  },
                  {
                    geminiPartner: US
                    submissionDeadlineOverride: "2025-07-31T10:00:01Z"
                  }
                ]
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) {
             callForProposals {
               id
               submissionDeadlineDefault
               partners {
                 geminiPartner
                 submissionDeadlineOverride
                 submissionDeadline
               }
               gemini {
                 allowsNonPartnerPi
                 nonPartnerDeadline
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
              "submissionDeadlineDefault": "2025-07-31T10:00:02Z",
              "partners": [
                {
                  "geminiPartner": "CA",
                  "submissionDeadlineOverride": "2025-07-31T10:00:00Z",
                  "submissionDeadline": "2025-07-31T10:00:00Z"
                },
                {
                  "geminiPartner": "CL",
                  "submissionDeadlineOverride": null,
                  "submissionDeadline": "2025-07-31T10:00:02Z"
                },
                {
                  "geminiPartner": "US",
                  "submissionDeadlineOverride": "2025-07-31T10:00:01Z",
                  "submissionDeadline": "2025-07-31T10:00:01Z"
                }
              ],
              "gemini": {
                "allowsNonPartnerPi": true,
                "nonPartnerDeadline": "2025-07-31T10:00:01Z"
              }
            }
          }
        }
      """.asRight
    )

  test("success - with empty partners"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2026-02-01"
                activeEnd:   "2026-07-31"
                partners:    []
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) {
             callForProposals {
               id
               partners { geminiPartner }
               gemini {
                 allowsNonPartnerPi
                 nonPartnerDeadline
               }
             }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id": "c-102",
              "partners": [],
              "gemini": {
                "allowsNonPartnerPi": false,
                "nonPartnerDeadline": null
              }
            }
          }
        }
      """.asRight
    )

  test("failure - with duplicate partners"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2026-02-01"
                activeEnd:   "2026-07-31"
                partners:    [
                  {
                    geminiPartner: US
                    submissionDeadlineOverride: "2025-07-31T10:00:00Z"
                  },
                  {
                    geminiPartner: US
                    submissionDeadlineOverride: "2025-07-31T10:00:01Z"
                  }
                ]
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) {
             callForProposals {
               id
               partners {
                 geminiPartner
                 submissionDeadline
               }
             }
          }
        }
      """,
      expected = List("Argument 'input.SET' is invalid: duplicate 'partners' specified: US").asLeft
    )

  test("success - with instruments"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2026-02-01"
                activeEnd:   "2026-07-31"
                gemini: {
                  type:        REGULAR_SEMESTER
                  instruments: [GMOS_SOUTH, GMOS_NORTH]
                }
              }
            }
          ) {
             callForProposals {
               gemini {
                 instruments
               }
             }
          }
        }
      """,
      // sorted by instrument name
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "gemini": {
                "instruments": [ "GMOS_NORTH", "GMOS_SOUTH" ]
              }
            }
          }
        }
      """.asRight
    )

  test("failure - with duplicate instruments"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2025A"
                activeStart: "2026-02-01"
                activeEnd:   "2026-07-31"
                gemini: {
                  type:        REGULAR_SEMESTER
                  instruments: [GMOS_SOUTH, GMOS_SOUTH]
                }
              }
            }
          ) {
             callForProposals {
               gemini {
                 instruments
               }
             }
          }
        }
      """,
      expected = List("Argument 'input.SET.gemini' is invalid: duplicate 'instruments' specified: GMOS_SOUTH").asLeft
    )

  test("success - with ra"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2024B"
                activeStart: "2024-07-31"
                activeEnd:   "2025-02-01"
                gemini: {
                  type: REGULAR_SEMESTER
                  coordinateLimits: {
                    north: {
                      raStart: { hms: "17:00:00" }
                      raEnd: { hms: "13:00:00" }
                    }
                  }
                }
              }
            }
          ) {
            callForProposals {
              gemini {
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
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "gemini": {
                "coordinateLimits": {
                  "north": {
                    "raStart": { "hms": "17:00:00.000000" },
                    "raEnd": { "hms": "13:00:00.000000" },
                    "decStart": { "dms": "-37:00:00.000000" },
                    "decEnd": { "dms": "+90:00:00.000000" }
                  },
                  "south": {
                    "raStart": { "hms": "15:30:00.000000" },
                    "raEnd": { "hms": "12:30:00.000000" },
                    "decStart": { "dms": "-90:00:00.000000" },
                    "decEnd": { "dms": "+28:00:00.000000" }
                  }
                }
              }
            }
          }
        }
      """.asRight
    )

  test("success - with dec"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2024B"
                activeStart: "2024-07-31"
                activeEnd:   "2025-02-01"
                gemini: {
                  type: REGULAR_SEMESTER
                  coordinateLimits: {
                    south: {
                      decStart: { dms: "45:00:00" }
                      decEnd: { dms: "-45:00:00" }
                    }
                  }
                }
              }
            }
          ) {
             callForProposals {
              gemini {
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
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "gemini": {
                "coordinateLimits": {
                  "north": {
                    "raStart": { "hms": "16:30:00.000000" },
                    "raEnd": { "hms": "14:00:00.000000" },
                    "decStart": { "dms": "-37:00:00.000000" },
                    "decEnd": { "dms": "+90:00:00.000000" }
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
        }
      """.asRight
    )

  test("director's time - non-partner accepted"):

    // existence defaults to PRESENT
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2024B"
                activeStart: "2024-07-31"
                activeEnd:   "2025-02-01"
                submissionDeadlineDefault: "2025-07-31T10:00:02Z"
                gemini: {
                  type: DIRECTORS_TIME
                }
              }
            }
          ) {
            callForProposals {
              id
              gemini {
                type
                allowsNonPartnerPi
                nonPartnerDeadline
                proprietaryMonths
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id":   "c-106",
              "gemini": {
                "type": "DIRECTORS_TIME",
                "allowsNonPartnerPi": true,
                "nonPartnerDeadline": "2025-07-31T10:00:02Z",
                "proprietaryMonths": 6
              }
            }
          }
        }
      """.asRight
    )

  test("demo science - non-partner prohibited"):

    // existence defaults to PRESENT
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2024B"
                activeStart: "2024-07-31"
                activeEnd:   "2025-02-01"
                submissionDeadlineDefault: "2025-07-31T10:00:02Z"
                gemini: {
                  type: DEMO_SCIENCE
                }
              }
            }
          ) {
            callForProposals {
              id
              gemini {
                type
                allowsNonPartnerPi
                nonPartnerDeadline
                proprietaryMonths
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "id":   "c-107",
              "gemini": {
                "type": "DEMO_SCIENCE",
                "allowsNonPartnerPi": false,
                "nonPartnerDeadline": null,
                "proprietaryMonths": 3
              }
            }
          }
        }
      """.asRight
    )

  test("negative LST bug"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester: "2024B"
                activeStart: "2024-10-01"
                activeEnd: "2024-12-31"
                submissionDeadlineDefault: "2024-08-31T22:00:00Z"
                partners: [{ geminiPartner: US }]
                gemini: {
                  type: FAST_TURNAROUND
                  instruments: [GMOS_NORTH, GMOS_SOUTH]
                }
              }
            }
          ) {
            callForProposals {
              gemini {
                coordinateLimits {
                  north {
                    raStart { hms }
                    raEnd { hms }
                  }
                  south {
                    raStart { hms }
                    raEnd { hms }
                  }
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
              "gemini": {
                "coordinateLimits": {
                  "north": {
                    "raStart": { "hms": "20:00:00.000000" },
                    "raEnd": { "hms": "12:00:00.000000" }
                  },
                  "south": {
                    "raStart": { "hms": "20:00:00.000000" },
                    "raEnd": { "hms": "10:00:00.000000" }
                  }
                }
              }
            }
          }
        }
      """.asRight
    )

  test("success - explicit proprietaryMonths"):
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:          "2024B"
                activeStart:       "2024-07-31"
                activeEnd:         "2025-02-01"
                gemini: {
                  type:              REGULAR_SEMESTER
                  proprietaryMonths: 36
                }
              }
            }
          ) {
            callForProposals {
              gemini {
                proprietaryMonths
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "gemini": {
                "proprietaryMonths": 36
              }
            }
          }
        }
      """.asRight
    )

  test("failure - too far in the future"):
    expect(
      user  = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester: "2099A"
                activeStart: "2026-02-01"
                activeEnd: "2026-07-31"
                partners: [{ geminiPartner: CL }, { geminiPartner: US }]
                gemini: {
                  type: REGULAR_SEMESTER
                  instruments: [GMOS_NORTH, GMOS_SOUTH]
                }
              }
            }
          ) {
            callForProposals {
              id
            }
          }
        }
      """,
      expected = List("The maximum semester is capped at the current year +1 (Semester(2099A) specified).").asLeft
    )

  test("success - maximum active period"):

    // existence defaults to PRESENT
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                semester:    "2024B"
                activeStart: "1901-01-01"
                activeEnd:   "2099-12-31"
                gemini: {
                  type: REGULAR_SEMESTER
                }
              }
            }
          ) {
            callForProposals {
              gemini {
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
              active {
                start
                end
              }
            }
          }
        }
      """,
      expected = json"""
        {
          "createCallForProposals": {
            "callForProposals": {
              "gemini": {
                "coordinateLimits": {
                  "north": {
                    "raStart": { "hms": "01:30:00.000000" },
                    "raEnd": { "hms": "12:00:00.000000" },
                    "decStart": { "dms": "-37:00:00.000000" },
                    "decEnd": { "dms": "+90:00:00.000000" }
                  },
                  "south": {
                    "raStart": { "hms": "03:30:00.000000" },
                    "raEnd": { "hms": "10:00:00.000000" },
                    "decStart": { "dms": "-90:00:00.000000" },
                    "decEnd": { "dms": "+28:00:00.000000" }
                  }
                }
              },
              "active": {
                "start": "1901-01-01",
                "end": "2099-12-31"
              }
            }
          }
        }
      """.asRight
    )