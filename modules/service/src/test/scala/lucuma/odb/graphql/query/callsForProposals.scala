// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.CallForProposals

class callsForProposals extends OdbSuite {

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
    ).flatMap {
      _.hcursor
       .downFields("createCallForProposals", "callForProposals", "id")
       .as[CallForProposals.Id]
       .leftMap(f => new RuntimeException(f.message))
       .liftTo[IO]
    }

  test("empty") {
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
  }

  test("WHERE id") {
    createCall(s"""
      type:        REGULAR_SEMESTER
      semester:    "2025A"
      activeStart: "2025-02-01 14:00:00"
      activeEnd:   "2025-07-31 14:00:00"
    """.stripMargin
    ).flatMap { id =>
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
    }
  }

  test("WHERE type") {
    createCall(s"""
      type:        FAST_TURNAROUND
      semester:    "2025A"
      activeStart: "2025-02-01 14:00:00"
      activeEnd:   "2025-07-31 14:00:00"
    """.stripMargin
    ).flatMap { id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: { type: { EQ: FAST_TURNAROUND } }) {
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
    }
  }

  test("WHERE semester") {
    createCall(s"""
      type:        REGULAR_SEMESTER
      semester:    "2024B"
      activeStart: "2025-02-01 14:00:00"
      activeEnd:   "2025-07-31 14:00:00"
    """.stripMargin
    ).flatMap { id =>
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
    }
  }

  test("WHERE active") {
    createCall(s"""
      type:        REGULAR_SEMESTER
      semester:    "2025B"
      activeStart: "2025-02-02 14:00:00"
      activeEnd:   "2025-07-30 14:00:00"
    """.stripMargin
    ).flatMap { id =>
      expect(pi,
        s"""
          query {
            callsForProposals(WHERE: {
              activeStart: { GT: "2025-02-01 14:00:00" }
              activeEnd:   { LT: "2025-07-31 14:00:00" }
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
    }
  }

  test("WHERE isOpen (none)") {
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
  }

  test("WHERE isOpen (no partners)") {
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
  }

  test("WHERE isOpen (active partner)") {
    createCall(s"""
      type:        DEMO_SCIENCE
      semester:    "2025B"
      activeStart: "2025-02-02 14:00:00"
      activeEnd:   "9999-07-30 14:00:00"
      partners:    [
        {
          partner: CA
          submissionDeadlineOverride: "3000-01-01 00:00:00"
        },
        {
          partner: US
          submissionDeadlineOverride: "2000-01-01 00:00:00"
        }
      ]
    """.stripMargin
    ).flatMap { id =>
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
    }
  }

}
