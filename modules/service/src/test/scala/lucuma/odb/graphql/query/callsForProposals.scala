// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.CallForProposals

class callsForProposals extends OdbSuite:

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(3, 103)

  override val validUsers = List(pi, staff)

  def createCall(set: String): IO[CallForProposals.Id] =
    query(
      user = staff,
      query = s"""
        mutation {
          createCallForProposals(
            input: {
              SET: {
                $set
              }
            }
          ) {
            callForProposals {
              id
            }
          }
        }
      """
    ).flatMap:
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "id")
       .as[CallForProposals.Id]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]

  test("empty"):
    expect(
      user  = pi,
      query = s"""
        query {
          callsForProposals() {
            hasMore
            matches {
              id
            }
          }
        }
      """,
      expected = json"""
        {
           "callsForProposals": {
             "hasMore": false,
             "matches": []
           }
        }
      """.asRight
    )

  test("WHERE id"):
    createCall(s"""
      semester:    "2025A"
      activeStart: "2025-02-01"
      activeEnd:   "2025-07-31"
      gemini: {
        type: REGULAR_SEMESTER
      }
    """.stripMargin
    ).flatMap: id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: { id: { EQ: "$id" } }) {
              matches {
                id
              }
            }
          }
        """,
        json"""
          {
            "callsForProposals": {
              "matches": [
                {
                  "id": $id
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE type"):
    createCall(s"""
      semester:    "2025A"
      activeStart: "2025-02-01"
      activeEnd:   "2025-07-31"
      gemini: {
        type: FAST_TURNAROUND
      }
    """.stripMargin
    ).flatMap: id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: { gemini: { type: { EQ: FAST_TURNAROUND } } }) {
              matches {
                id
              }
            }
          }
        """,
        json"""
          {
            "callsForProposals": {
              "matches": [
                {
                  "id": $id
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE semester"):
    createCall(s"""
      semester:    "2024B"
      activeStart: "2025-02-01"
      activeEnd:   "2025-07-31"
      gemini: {
        type: REGULAR_SEMESTER
      }
    """.stripMargin
    ).flatMap: id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: { semester: { LT: "2025A" } }) {
              matches {
                id
              }
            }
          }
        """,
        json"""
          {
            "callsForProposals": {
              "matches": [
                {
                  "id": $id
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE active"):
    createCall(s"""
      semester:    "2025B"
      activeStart: "2025-02-02"
      activeEnd:   "2025-07-30"
      gemini: {
        type: REGULAR_SEMESTER
      }
    """.stripMargin
    ).flatMap: id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: {
              activeStart: { GT: "2025-02-01" }
              activeEnd:   { LT: "2025-07-31" }
            }) {
              matches {
                id
              }
            }
          }
        """,
        json"""
          {
            "callsForProposals": {
              "matches": [
                {
                  "id": $id
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE isOpen (none)"):
    expect(pi,
      s"""
        query {
          callsForProposals(WHERE: { isOpen: { EQ: true } } ) {
            matches {
              id
            }
          }
        }
      """,
      json"""
        {
          "callsForProposals": {
            "matches": []
          }
        }
      """.asRight
    )

  test("WHERE isOpen (no partners)"):
    expect(pi,
      s"""
        query {
          callsForProposals(WHERE: { isOpen: { EQ: false } } ) {
            matches {
              id
            }
          }
        }
      """,
      json"""
        {
          "callsForProposals": {
            "matches": [
              {
                "id": "c-100"
              },
              {
                "id": "c-101"
              },
              {
                "id": "c-102"
              },
              {
                "id": "c-103"
              }
            ]
          }
        }
      """.asRight
    )

  test("WHERE isOpen (active partner)"):
    createCall(s"""
      semester:    "2025B"
      activeStart: "2025-02-02"
      activeEnd:   "2099-07-30"
      partners:    [
        {
          geminiPartner: CA
          submissionDeadlineOverride: "3000-01-01 00:00:00"
        },
        {
          geminiPartner: US
          submissionDeadlineOverride: "2000-01-01 00:00:00"
        }
      ]
      gemini: {
        type: DEMO_SCIENCE
      }
    """.stripMargin
    ).flatMap: id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: { isOpen: { EQ: true } }) {
              matches {
                id
              }
            }
          }
        """,
        json"""
          {
            "callsForProposals": {
              "matches": [
                {
                  "id": $id
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE allowsNonPartnerPi"):
    expect(pi,
      s"""
        query {
          callsForProposals(WHERE: { gemini: { allowsNonPartnerPi: { EQ: true } } }) {
            matches {
              id
            }
          }
        }
      """,
      json"""
        {
          "callsForProposals": {
            "matches": [
              {
                "id": "c-100"
              },
              {
                "id": "c-102"
              },
              {
                "id": "c-103"
              }
            ]
          }
        }
      """.asRight
    )

  test("WHERE observatory"):
    // No prior test creates a Keck call, so filtering on KECK isolates this one.
    createCall(s"""
      semester:    "2025A"
      activeStart: "2025-02-01"
      activeEnd:   "2025-07-31"
      keck: {
        instruments: [ HIRES ]
      }
    """.stripMargin
    ).flatMap: id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: { observatory: { EQ: KECK } }) {
              matches {
                id
                observatory
              }
            }
          }
        """,
        json"""
          {
            "callsForProposals": {
              "matches": [
                {
                  "id": $id,
                  "observatory": "KECK"
                }
              ]
            }
          }
        """.asRight
      )

  test("WHERE query contains unmatched observatories"):
    expect(
      user  = pi,
      query = s"""
        query {
          callsForProposals(WHERE: { observatory: { EQ: KECK } }) {
            matches {
              observatory
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
                allowsNonPartnerPi
                nonPartnerDeadline
                exchangePartners
              }
              keck {
                instruments
                coordinateLimits {
                  raStart { hms }
                  raEnd { hms }
                  decStart { dms }
                  decEnd { dms }
                }
              }
              subaru {
                type
                instruments
                coordinateLimits {
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
          "callsForProposals": {
            "matches": [
              {
                "observatory": "KECK",
                "gemini": null,
                "keck": {
                  "instruments": [ "HIRES" ],
                  "coordinateLimits": {
                    "raStart": {
                      "hms": "04:00:00.000000"
                    },
                    "raEnd": {
                      "hms": "01:00:00.000000"
                    },
                    "decStart" : {
                      "dms" : "-37:00:00.000000"
                    },
                    "decEnd" : {
                      "dms" : "+90:00:00.000000"
                    }
                  }
                },
                "subaru": null
              }
            ]
          }
        }
      """.asRight
    )