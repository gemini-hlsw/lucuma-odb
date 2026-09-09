// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service
package graphql
package query

import io.circe.Json
import io.circe.literal.*

class apiKeys extends GraphQLSuite with SsoSuite with Fixture:

  test("Empty apiKeys with nested object selections."):
    AsAlice.viaJwt.expectQuery(
      query = """
        query {
          user {
            apiKeys {
              id
              role {
                type
              }
              user {
                id
              }
            }
          }
        }
      """,
      expected = json"""{
        "data" : {
          "user" : {
            "apiKeys" : []
          }
        }
      }"""
    )

  test("Empty apiKeys without nested object selections."):
    AsAlice.viaJwt.expectQuery(
      query = """
        query {
          user {
            apiKeys {
              id
            }
          }
        }
      """,
      expected = json"""{
        "data" : {
          "user" : {
            "apiKeys" : []
          }
        }
      }"""
    )

  test("Non-empty apiKeys with nested object selections."):
    AsBob.query(
      """
        query {
          user {
            id
            apiKeys {
              id
              role {
                type
              }
              user {
                id
              }
            }
          }
        }
      """
    ).map: json =>
      val user = json.hcursor.downFields("data", "user")
      val uid  = user.downField("id").require[String]
      val keys = user.downField("apiKeys").require[List[Json]]
      assert(keys.nonEmpty)
      keys.foreach: key =>
        assertEquals(key.hcursor.downFields("role", "type").require[String], "PI")
        assertEquals(key.hcursor.downFields("user", "id").require[String], uid)

  // Sorting by `id` must work even when the client does not select `id`.
  test("Non-empty apiKeys without selecting id."):
    AsBob.query(
      """
        query {
          user {
            apiKeys {
              role {
                type
              }
            }
          }
        }
      """
    ).map: json =>
      val keys = json.hcursor.downFields("data", "user", "apiKeys").require[List[Json]]
      assert(keys.nonEmpty)
      keys.foreach: key =>
        assertEquals(key.hcursor.downFields("role", "type").require[String], "PI")
