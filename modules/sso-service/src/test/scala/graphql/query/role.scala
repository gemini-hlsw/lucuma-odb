// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import io.circe.literal.*

object role extends GraphQLSuite with SsoSuite with Fixture with FlakyTests:

  test("Query current role."):
    flaky():
      As(Bob).expectQuery(
        query = """
          query {
            role {
              type
              partner
              user {
                profile {
                  givenName
                  familyName
                  creditName
                  email
                }
              }
            }
          }
        """,
        expected = json"""{
          "data" : {
            "role" : {
              "type" : "PI",
              "partner" : null,
              "user" : {
                "profile": {
                  "givenName" : "Bob",
                  "familyName" : "Dobbs",
                  "creditName" : null,
                  "email" : "bob@dobbs.com"
                }
              }
            }
          }
        }"""
      )


