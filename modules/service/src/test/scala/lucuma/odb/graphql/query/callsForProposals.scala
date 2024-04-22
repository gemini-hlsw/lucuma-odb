// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*

class callsForProposals extends OdbSuite {

  val pi   = TestUsers.Standard.pi(1, 30)

  override val validUsers = List(pi).toList

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

  // TODO: more tests when we have a WHERE clause
}
