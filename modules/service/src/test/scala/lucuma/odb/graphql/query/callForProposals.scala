// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*
import io.circe.syntax.*
import lucuma.odb.data.CallForProposals

class callForProposals extends OdbSuite {

  val pi    = TestUsers.Standard.pi(1, 30)
  val staff = TestUsers.Standard.staff(3, 103)
  override val validUsers = List(pi, staff)

  private val createCall: IO[CallForProposals.Id] =
    query(
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

  test("callForProposals - null") {
    expect(
      user  = pi,
      query = s"""
        query {
          callForProposals(callForProposalsId: "c-123") {
            id
          }
        }
      """,
      expected = json"""
        {
           "callForProposals": null
        }
      """.asRight
    )
  }

  test("callForProposals - lookup") {
    createCall.flatMap { cid =>
      expect(
        user  = pi,
        query = s"""
          query {
            callForProposals(callForProposalsId: "$cid") {
              id
            }
          }
        """,
        expected = json"""
          {
            "callForProposals": {
              "id": ${cid.asJson}
            }
          }
        """.asRight
      )
    }
  }

}
