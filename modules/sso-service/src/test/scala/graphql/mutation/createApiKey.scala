// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package mutation

import cats.syntax.show.*
import io.circe.Json
import io.circe.literal.*
import lucuma.sso.client.ApiKey
import lucuma.sso.service.database.RoleRequest

class createApiKey extends GraphQLSuite with SsoSuite with Fixture:

  def isApiKey(json: Json): Boolean =
    json
      .hcursor
      .downFields("data", "createApiKey")
      .as[String]
      .toOption
      .map(ApiKey.fromString.getOption)
      .isDefined

  test("Attempt to create API key with invalid role id"):
    As(Bob).viaJwt.expectQuery(
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
    As(Bob).viaJwt.expectQuery(
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
    As(Bob).viaJwt.queryWithUser { user =>
      show"""
        mutation {
          createApiKey(role: "${user.role.id}")
        }
      """
    } .map(json => assert(isApiKey(json)))

  test("Create an API key for another owned role"):
    As(Bob).withRoleRequest(RoleRequest.Staff).viaJwt.queryWithUser { user =>
      show"""
        mutation {
          createApiKey(role: "${user.otherRoles.head.id}")
        }
      """
    } .map(json => assert(isApiKey(json)))

  test("An API key can create an API key for its own role"):
    As(Bob).queryWithUser { user =>
      show"""
        mutation {
          createApiKey(role: "${user.role.id}")
        }
      """
    } .map(json => assert(isApiKey(json)))

  test("An API key cannot create an API key for another owned role"):
    As(Bob).withRoleRequest(RoleRequest.Staff).expectQueryWithUser(
      query = user =>
        show"""
          mutation {
            createApiKey(role: "${user.otherRoles.head.id}")
          }
        """,
      expected = json"""{
        "errors" : [
          {
            "message" : "An API key may only create API keys for its own role."
          }
        ],
        "data" : null
      }"""
    )
