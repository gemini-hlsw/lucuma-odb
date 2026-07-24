// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package mutation

import cats.syntax.show.*
import io.circe.literal.*
import lucuma.sso.client.ApiKey

class createApiKey extends GraphQLSuite with SsoSuite with Fixture with FlakyTests:

  test("Attempt to create API key with invalid role id"):
    flaky():
      As(Bob).expectQuery(
        query = """
          mutation {
            createApiKey(role: "bogus")
          }
        """,
        expected = json"""{
          "errors" : [
            {
              "message" : "Not a valid role id: bogus"
            }
          ]
        }"""
      )

  test("Attempt to create API key with a role we don't own"):
    flaky():
      As(Bob).expectQuery(
        query = """
          mutation {
            createApiKey(role: "r-99999")
          }
        """,
        expected = json"""{
          "errors" : [
            {
              "message" : "No such role: r-99999"
            }
          ],
          "data" : null
        }"""
      )

  test("Create an API key"):
    flaky():
      As(Bob).queryWithUser { user =>
        show"""
          mutation {
            createApiKey(role: "${user.role.id}")
          }
        """
      } .map { json =>
        assert:
          json
            .hcursor
            .downFields("data", "createApiKey")
            .as[String]
            .toOption
            .map(ApiKey.fromString.getOption)
            .isDefined
      }

