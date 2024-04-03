// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.syntax.either.*
import io.circe.Json
import io.circe.literal.*

class callForProposals extends OdbSuite {

  val pi   = TestUsers.Standard.pi(1, 30)

  override val validUsers = List(pi).toList

  test("callForProposals - empty") {
    expect(
      user  = pi,
      query = s"""
        query {
          callForProposals() {
            hasMore
            matches {
              id
            }
          }
        }
      """,
      expected = json"""
        {
           "callForProposals": {
             "hasMore": false,
             "matches": []
           }
        }
      """.asRight
    )
  }
}
