// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package mutation

import io.circe.literal.*

class deleteApiKey extends GraphQLSuite with SsoSuite with Fixture:

  test("Attempt to delete API key with invalid id"):
    As(Bob).viaJwt.expectQuery(
      query = """
        mutation {
          deleteApiKey(id: "bogus")
        }
      """,
      expected = json"""{
        "errors" : [
          {
            "message" : "Not a valid API key id: bogus"
          }
        ]
      }"""
    )

  test("Delete a nonexistent API key with a session token"):
    As(Bob).viaJwt.expectQuery(
      query = """
        mutation {
          deleteApiKey(id: "ffffff")
        }
      """,
      expected = json"""{
        "data" : {
          "deleteApiKey" : false
        }
      }"""
    )

  test("An API key cannot delete API keys"):
    As(Bob).expectQuery(
      query = """
        mutation {
          deleteApiKey(id: "ffffff")
        }
      """,
      expected = json"""{
        "errors" : [
          {
            "message" : "API keys cannot delete API keys. Authenticate with a session token."
          }
        ],
        "data" : null
      }"""
    )
