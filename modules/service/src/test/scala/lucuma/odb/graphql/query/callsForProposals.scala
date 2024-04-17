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
  /*
                type:        REGULAR_SEMESTER
                semester:    "2025A"
                activeStart: "2025-02-01 14:00:00"
                activeEnd:   "2025-07-31 14:00:00"
   */

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

  test("callsForProposals - empty") {
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

}
