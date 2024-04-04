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

  test("successful creation") {
    expect(
      user = staff,
      query = """
        mutation {
          createCallForProposals(
            input: {
              SET: {
                type: REGULAR_SEMESTER
                semester: "2025A"
                activeStart: "2026-02-01 14:00:00"
                activeEnd: "2026-07-31 14:00:00"
              }
            }
          ) {
            callForProposals {
              status
              type
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
              "status": "CLOSED",
              "type":   "REGULAR_SEMESTER",
              "activeStart": "2026-02-01 14:00:00",
              "activeEnd": "2026-07-31 14:00:00",
              "existence": "PRESENT"
            }
          }
        }
      """.asRight
    )
  }
}
