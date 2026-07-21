// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import io.circe.literal.*

object users extends GraphQLSuite with SsoSuite with Fixture with FlakyTests:

  test("Query all users."):
    flaky():
      As(Bob).expectQuery(
        query = """
          query {
            users(LIMIT: 3) {
              matches {
                id
                orcidId
                type
                enabled
                roles {
                  type              
                }
              }
            }
          }
        """,
        expected = json"""42"""
      )


