// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.CallForProposals
import lucuma.core.model.User

class updateCallsForProposals extends OdbSuite {

  val service    = TestUsers.service(3)
  val staff      = TestUsers.Standard.staff(4, 44)
  val validUsers = List(service, staff)

  val createCall: IO[CallForProposals.Id] =
    query(
      staff,
      s"""
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

  test("type") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                type: DEMO_SCIENCE
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                type
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "type": "DEMO_SCIENCE"
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("semester") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                semester: "2024B"
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                semester
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "semester": "2024B"
                }
              ]
            }
          }
        """.asRight
      )
    }
  }

  test("existence") {
    createCall.flatMap { id =>
      expect(
        staff,
        s"""
          mutation {
            updateCallsForProposals(input: {
              SET: {
                existence: DELETED
              },
              WHERE: {
                id: { EQ: "$id" }
              }
            }) {
              callsForProposals {
                existence
              }
            }
          }
        """,
        json"""
          {
            "updateCallsForProposals": {
              "callsForProposals": [
                {
                  "existence": "DELETED"
                }
              ]
            }
          }
        """.asRight
      )
    }
  }
}
